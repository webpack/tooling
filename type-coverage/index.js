// loosely based on https://github.com/plantain-00/type-coverage
const { root } = require("../lib/argv");
const path = require("path");
const fs = require("fs");
const ts = require("typescript");
const program = require("../lib/typescript-program");

const typeChecker = program.getTypeChecker();

/**
 * @typedef {Object} Location
 * @property {number} line
 * @property {number} column
 */

/**
 * @typedef {Object} FileReport
 * @property {string} path
 * @property {Record<number, { start: Location, end: Location }>} statementMap
 * @property {{}} fnMap
 * @property {{}} branchMap
 * @property {Record<number, number>} s
 * @property {{}} f
 * @property {{}} b
 */

/** @type {Record<string, FileReport>} */
const coverageReport = Object.create(null);

/**
 * IMPROVEMENT: Helper to check if a type is 'any' or contains 'any' (e.g., in a union)
 * Using bitwise flags is significantly faster than converting types to strings.
 * @param {ts.Type} type 
 * @returns {boolean}
 */
const isTypeMissing = (type) => {
    if (!type) return false;
    // Check if the type is explicitly 'any'
    if (type.flags & ts.TypeFlags.Any) return true;
    
    // Recursively check unions (e.g., string | any)
    if (type.isUnion()) {
        return type.types.some(isTypeMissing);
    }
    // Recursively check intersections (e.g., T & any)
    if (type.isIntersection()) {
        return type.types.some(isTypeMissing);
    }
    return false;
};

for (const sourceFile of program.getSourceFiles()) {
    let file = sourceFile.fileName;
    if (!sourceFile.isDeclarationFile) {
        /** @type {FileReport} */
        const rep = {
            path: path.sep !== "/" ? file.replace(/\//g, path.sep) : file,
            statementMap: {},
            fnMap: {},
            branchMap: {},
            s: {},
            f: {},
            b: {},
        };
        coverageReport[rep.path] = rep;
        let statementIndex = 0;

        /**
         * @param {ts.Node} node the node to be walked
         * @returns {boolean}
         */
        const isReplaceMethod = (node) => {
            if (!ts.isIdentifier(node) || node.text !== "replace") return false;
            if (!ts.isPropertyAccessExpression(node.parent) || node.parent.name !== node) return false;

            const callExpr = node.parent.parent;
            if (!ts.isCallExpression(callExpr)) return false;

            const objType = typeChecker.getTypeAtLocation(node.parent.expression);
            const isString = (objType.getFlags() & (ts.TypeFlags.String | ts.TypeFlags.StringLiteral)) !== 0;

            if (!isString || callExpr.arguments.length < 2) return false;

            const secondArg = callExpr.arguments[1];
            if (ts.isStringLiteral(secondArg) || ts.isNoSubstitutionTemplateLiteral(secondArg) || ts.isTemplateExpression(secondArg)) {
                return true;
            }

            const type = typeChecker.getTypeAtLocation(secondArg);
            return type.getCallSignatures().some(sig => sig.getParameters().length === 1);
        };

        /**
         * @param {ts.Node} node the node to be walked
         * @returns {void}
         */
        const walkNode = (node) => {
            if (ts.isIdentifier(node) || node.kind === ts.SyntaxKind.ThisKeyword) {
                // IMPROVEMENT: Fetch type once and reuse
                const type = typeChecker.getTypeAtLocation(node);
                if (!type) return;

                const { line, character } = ts.getLineAndCharacterOfPosition(sourceFile, node.getStart());
                const { line: lineEnd, character: characterEnd } = ts.getLineAndCharacterOfPosition(sourceFile, node.getEnd());

                let isExternal = false;
                const checkDecls = (t) => {
                    if (!t.symbol) return;
                    for (const decl of t.symbol.getDeclarations()) {
                        const sf = decl.getSourceFile();
                        if (sf && sf.isDeclarationFile) isExternal = true;
                    }
                };

                if (node.parent && ts.isPropertyAccessExpression(node.parent)) {
                    checkDecls(typeChecker.getTypeAtLocation(node.parent.expression));
                }

                // IMPROVEMENT: We no longer need the expensive typeToString() for the 'any' check
                const typeText = typeChecker.typeToString(type);
                if (/^(<.*>)?\(/.test(typeText)) {
                    checkDecls(type);
                }

                const isTyped =
                    ((ts.isLabeledStatement(node.parent) || ts.isBreakStatement(node.parent) || ts.isContinueStatement(node.parent)) && node.parent.label === node) ||
                    (ts.isBindingElement(node.parent) && node.parent.propertyName === node) ||
                    isReplaceMethod(node) ||
                    isExternal ||
                    // IMPROVEMENT: Replaced regex check with high-performance flag check
                    !isTypeMissing(type);

                rep.statementMap[statementIndex] = {
                    start: { line: line + 1, column: character },
                    end: { line: lineEnd + 1, column: characterEnd - 1 },
                };
                // Store length if typed, 0 if 'any'
                rep.s[statementIndex] = isTyped ? typeText.length : 0;
                statementIndex++;
            }

            node.forEachChild(walkNode);
        };

        sourceFile.forEachChild(walkNode);
    }
}

const outputDirectory = path.resolve(root, "coverage");
fs.mkdirSync(outputDirectory, { recursive: true });
fs.writeFileSync(
    path.resolve(outputDirectory, "coverage-types.json"),
    JSON.stringify(coverageReport),
    "utf-8",
);
