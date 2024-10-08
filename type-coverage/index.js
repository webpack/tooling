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
		 * @returns {void}
		 */
		const walkNode = (node) => {
			if (ts.isIdentifier(node) || node.kind === ts.SyntaxKind.ThisKeyword) {
				const type = typeChecker.getTypeAtLocation(node);
				if (type) {
					const { line, character } = ts.getLineAndCharacterOfPosition(
						sourceFile,
						node.getStart(),
					);
					const { line: lineEnd, character: characterEnd } =
						ts.getLineAndCharacterOfPosition(sourceFile, node.getEnd());
					const typeText = typeChecker.typeToString(type);
					let isExternal = false;

					/**
					 * @param {ts.Type} type the type to be checked
					 * @returns {void}
					 */
					const checkDecls = (type) => {
						if (!type.symbol) return;
						for (const decl of type.symbol.getDeclarations()) {
							const sourceFile = decl.getSourceFile();
							if (sourceFile && sourceFile.isDeclarationFile) isExternal = true;
						}
					};
					if (node.parent && ts.isPropertyAccessExpression(node.parent)) {
						const expressionType = typeChecker.getTypeAtLocation(
							node.parent.expression,
						);
						checkDecls(expressionType);
					}
					if (/^(<.*>)?\(/.test(typeText)) {
						checkDecls(type);
					}
					const isTyped =
						isExternal ||
						(!(type.flags & ts.TypeFlags.Any) && !/\bany\b/.test(typeText));
					rep.statementMap[statementIndex] = {
						start: {
							line: line + 1,
							column: character,
						},
						end: {
							line: lineEnd + 1,
							column: characterEnd - 1,
						},
					};
					rep.s[statementIndex] = isTyped ? typeText.length : 0;
					statementIndex++;
				}
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
