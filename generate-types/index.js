const argv = require("../lib/argv");

const { write: doWrite, verbose, root, types: outputFile } = argv;

const path = require("path");
const fs = require("fs").promises;
const ts = require("typescript");
const prettier = require("prettier");

process.exitCode = 1;
let exitCode = 0;

const AnonymousType = "__Type";

const IDENTIFIER_NAME_REPLACE_REGEX = /^([^a-zA-Z$_])/;
const IDENTIFIER_ALPHA_NUMERIC_NAME_REPLACE_REGEX = /[^a-zA-Z0-9$]+/g;
const toIdentifier = (str) => {
	return str
		.replace(IDENTIFIER_NAME_REPLACE_REGEX, "_$1")
		.replace(IDENTIFIER_ALPHA_NUMERIC_NAME_REPLACE_REGEX, "_");
};

const joinIdentifer = (list) => {
	const str = list.join("_");
	return str.replace(/([^_])_+(.|$)/g, (m, a, b) =>
		a !== a.toLowerCase() ? `${a}_${b}` : a + b.toUpperCase()
	);
};

const flatten = (iterable) => {
	const array = [];
	for (const list of iterable) {
		array.push(...list);
	}
	return array;
};

const quoteMeta = (str) => {
	return str.replace(/[-[\]\\/{}()*+?.^$|]/g, "\\$&");
};

class TupleMap {
	constructor() {
		/** @type {Map<any, { map: TupleMap | undefined, hasValue: boolean, value: any }>} */
		this.map = new Map();
	}

	/**
	 * @param {any[]} tuple tuple
	 * @returns {any} value or undefined
	 */
	get(tuple) {
		return this._get(tuple, 0);
	}

	_get(tuple, index) {
		const entry = this.map.get(tuple[index]);
		if (entry === undefined) return undefined;
		if (tuple.length === index + 1) return entry.value;
		if (entry.map === undefined) return undefined;
		return entry.map._get(tuple, index + 1);
	}

	/**
	 * @param {any[]} tuple tuple
	 * @returns {boolean} true, if it's in the map
	 */
	has(tuple) {
		return this._has(tuple, 0);
	}

	_has(tuple, index) {
		const entry = this.map.get(tuple[index]);
		if (entry === undefined) return false;
		if (tuple.length === index + 1) return entry.hasValue;
		if (entry.map === undefined) return false;
		return entry.map._has(tuple, index + 1);
	}

	/**
	 * @param {any[]} tuple tuple
	 * @param {any} value the new value
	 * @returns {void}
	 */
	set(tuple, value) {
		return this._set(tuple, 0, value);
	}

	_set(tuple, index, value) {
		let entry = this.map.get(tuple[index]);
		if (entry === undefined) {
			entry = { map: undefined, hasValue: false, value: undefined };
			this.map.set(tuple[index], entry);
		}
		if (tuple.length === index + 1) {
			entry.hasValue = true;
			entry.value = value;
			return;
		}
		if (entry.map === undefined) {
			entry.map = new TupleMap();
		}
		entry.map._set(tuple, index + 1, value);
	}

	/**
	 * @param {any[]} tuple tuple
	 * @returns {void}
	 */
	add(tuple) {
		return this._add(tuple, 0);
	}

	_add(tuple, index) {
		let entry = this.map.get(tuple[index]);
		if (entry === undefined) {
			entry = { map: undefined, hasValue: false, value: undefined };
			this.map.set(tuple[index], entry);
		}
		if (tuple.length === index + 1) {
			entry.hasValue = true;
			entry.value = undefined;
			return;
		}
		if (entry.map === undefined) {
			entry.map = new TupleMap();
		}
		entry.map._add(tuple, index + 1);
	}

	/**
	 * @param {any[]} tuple tuple
	 * @returns {void}
	 */
	delete(tuple) {
		return this._delete(tuple, 0);
	}

	_delete(tuple, index) {
		const entry = this.map.get(tuple[index]);
		if (entry === undefined) {
			return;
		}
		if (tuple.length === index + 1) {
			entry.hasValue = false;
			entry.value = undefined;
			if (entry.map === undefined) {
				this.map.delete(tuple[index]);
			}
			return;
		}
		if (entry.map === undefined) {
			return;
		}
		entry.map._delete(tuple, index + 1);
		if (entry.map.map.size === 0) {
			entry.map = undefined;
			if (!entry.hasValue) {
				this.map.delete(tuple[index]);
			}
		}
	}

	values() {
		const values = [];
		for (const entry of this.map.values()) {
			if (entry.hasValue) values.push(entry.value);
			if (entry.map !== undefined) {
				values.push(...entry.map.values());
			}
		}
		return values;
	}
}

/**
 * @param {ts.Diagnostic} diagnostic info
 * @returns {void}
 */
const printError = (diagnostic) => {
	if (diagnostic.file && typeof diagnostic.start === "number") {
		let { line, character } = diagnostic.file.getLineAndCharacterOfPosition(
			diagnostic.start
		);
		let message = ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n");
		console.error(
			`${diagnostic.file.fileName} (${line + 1},${character + 1}): ${message}`
		);
	} else {
		console.error(
			ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n")
		);
	}
};

(async () => {
	const rootPath = path.resolve(root);
	const configPath = path.resolve(rootPath, "tsconfig.types.json");
	const configContent = ts.sys.readFile(configPath);
	if (!configContent) {
		console.error("Empty config file");
		return;
	}
	const configJsonFile = ts.parseJsonText(configPath, configContent);
	const parsedConfig = ts.parseJsonSourceFileConfigFileContent(
		configJsonFile,
		ts.sys,
		rootPath,
		{
			noEmit: true,
		}
	);

	if (parsedConfig.errors && parsedConfig.errors.length > 0) {
		for (const error of parsedConfig.errors) {
			printError(error);
		}
		return;
	}

	const program = ts.createProgram(
		parsedConfig.fileNames,
		parsedConfig.options
	);

	const diagnostics = ts.getPreEmitDiagnostics(program);

	if (diagnostics.length > 0) {
		for (const diagnostic of diagnostics) {
			printError(diagnostic);
		}
		// return;
	}

	const checker = program.getTypeChecker();

	const exposedFiles = ["lib/index.js"];

	/** @type {Set<ts.Type>} */
	const collectedTypes = new Set();
	/** @type {Map<ts.Type, { source: ts.SourceFile, symbol?: ts.Symbol, name?: string }>} */
	const typeNameHints = new Map();
	/** @type {Map<ts.Type, Set<ts.Type>>} */
	const typeReferencedBy = new Map();

	/**
	 * @param {ts.Type | undefined} from source type
	 * @param {ts.Type} type type
	 * @param {ts.SourceFile=} source source file
	 * @param {ts.Symbol=} symbol declaration symbol
	 * @param {string=} name name hint
	 * @returns {void}
	 */
	const captureType = (from, type, source, symbol, name) => {
		if (from) {
			const set = typeReferencedBy.get(type);
			if (set === undefined) {
				typeReferencedBy.set(type, new Set([from]));
			} else {
				set.add(from);
			}
		}
		if (typeNameHints.has(type)) return;
		collectedTypes.add(type);
		const hint = typeNameHints.get(type);
		if (!hint) return;
		typeNameHints.set(type, {
			source: source || hint.source,
			symbol: symbol || hint.symbol,
			name: name || hint.name,
		});
	};

	/**
	 * @param {ts.SourceFile} source source file
	 * @returns {ts.Type} the type
	 */
	const getTypeOfSourceFile = (source) => {
		const sourceAsAny = /** @type {any} */ (source);
		if (
			sourceAsAny.externalModuleIndicator ||
			sourceAsAny.commonJsModuleIndicator
		) {
			const moduleSymbol = /** @type {ts.Symbol} */ (sourceAsAny.symbol);
			return checker.getTypeOfSymbolAtLocation(moduleSymbol, source);
		}
		throw new Error("Not a module");
	};

	/**
	 * @param {ts.SourceFile} source source file
	 * @returns {boolean} true when it's a module
	 */
	const isSourceFileModule = (source) => {
		const sourceAsAny = /** @type {any} */ (source);
		return (
			sourceAsAny.externalModuleIndicator || sourceAsAny.commonJsModuleIndicator
		);
	};

	/**
	 * @param {ts.SourceFile} source source file
	 * @returns {Map<ts.Symbol, string>} exposed symbols with their names
	 */
	const getExportsOfSourceFile = (source) => {
		// TODO caching
		/** @type Map<ts.Symbol, string> */
		const map = new Map();
		const sourceAsAny = /** @type {any} */ (source);
		if (isSourceFileModule(source)) {
			const moduleSymbol = /** @type {ts.Symbol} */ (sourceAsAny.symbol);
			if (moduleSymbol && moduleSymbol.exports) {
				moduleSymbol.exports.forEach((symbol, name) => {
					map.set(symbol, ts.unescapeLeadingUnderscores(name));
				});
			}
		} else {
			for (const [name, symbol] of sourceAsAny.locals) {
				map.set(symbol, name);
			}
		}
		// Expand namespaces
		for (const [symbol, namespaceName] of map) {
			const flags = symbol.getFlags();
			if (
				flags & ts.SymbolFlags.NamespaceModule ||
				flags & ts.SymbolFlags.ValueModule
			) {
				if (symbol.exports) {
					symbol.exports.forEach((symbol, name) => {
						map.set(
							symbol,
							`${namespaceName}.${ts.unescapeLeadingUnderscores(name)}`
						);
					});
				}
			}
		}
		// Expand references
		for (const [symbol, name] of map) {
			const decl = getDeclaration(symbol);
			if (decl.expression) {
				const type = checker.getTypeAtLocation(decl.expression);
				if (type && type.symbol && !map.has(type.symbol))
					map.set(type.symbol, name);
			}
		}
		return map;
	};

	let exposedType;

	for (const exposedFile of exposedFiles) {
		const exposedSource = program.getSourceFile(
			path.resolve(rootPath, exposedFile)
		);
		if (!exposedSource) {
			console.error(
				`No source found for ${exposedFile}. These files are available:`
			);
			for (const source of program.getSourceFiles()) {
				console.error(` - ${source.fileName}`);
			}
			continue;
		}

		const type = getTypeOfSourceFile(exposedSource);
		captureType(undefined, type, exposedSource);
		exposedType = type;
	}

	const getDeclaration = (symbol) => {
		if (!symbol) return undefined;
		/** @type {ts.Declaration | undefined} */
		let decl = undefined;
		if (symbol.valueDeclaration) {
			decl = symbol.valueDeclaration;
		} else {
			const decls = symbol.getDeclarations();
			if (decls) decl = decls[0];
		}
		if (decl) {
			const symbol = /** @type {any} */ (decl).symbol;
			if (symbol && symbol.name === ts.InternalSymbolName.Type && decl.parent) {
				const parent = decl.parent;
				if (parent.kind === ts.SyntaxKind.TypeAliasDeclaration) {
					decl = /** @type {ts.Declaration} */ (parent);
				}
				if (
					parent.kind === ts.SyntaxKind.JSDocTypeExpression &&
					parent.parent &&
					parent.parent.kind === ts.SyntaxKind.JSDocTypedefTag
				) {
					decl = /** @type {ts.Declaration} */ (parent.parent);
				}
			}
		}
		return decl;
	};

	/** @typedef {{ signature: ts.Signature, typeParameters?: readonly ts.Type[], args: { name: string, optional: boolean, spread: boolean, type: ts.Type }[], returnType: ts.Type }} ParsedSignature */
	/** @typedef {string[]} SymbolName */
	/** @typedef {Map<string, { type: ts.Type, method: boolean, optional: boolean, readonly: boolean }>} PropertiesMap */

	/** @typedef {{ type: "primitive", name: string }} ParsedPrimitiveType */
	/** @typedef {{ type: "tuple", typeArguments: readonly ts.Type[] }} ParsedTupleType */
	/** @typedef {{ type: "interface", symbolName: SymbolName, class: boolean, properties: PropertiesMap, constructors: ParsedSignature[], calls: ParsedSignature[], numberIndex?: ts.Type, stringIndex?: ts.Type, typeParameters?: readonly ts.Type[], baseTypes: readonly ts.Type[] }} ParsedInterfaceType */
	/** @typedef {{ type: "class" | "typeof class", symbolName: SymbolName, properties: PropertiesMap, staticProperties: PropertiesMap, constructors: ParsedSignature[], numberIndex?: ts.Type, stringIndex?: ts.Type, typeParameters?: readonly ts.Type[], baseType: ts.Type, correspondingType: ts.Type | undefined }} MergedClassType */
	/** @typedef {{ type: "reference", target: ts.Type, typeArguments: readonly ts.Type[] }} ParsedReferenceType */
	/** @typedef {{ type: "union", symbolName: SymbolName, types: ts.Type[] }} ParsedUnionType */
	/** @typedef {{ type: "intersection", symbolName: SymbolName, types: ts.Type[] }} ParsedIntersectionType */
	/** @typedef {{ type: "import", symbolName: SymbolName, exportName: string, from: string }} ParsedImportType */
	/** @typedef {{ type: "symbol", symbolName: SymbolName }} ParsedSymbolType */
	/** @typedef {ParsedPrimitiveType | ParsedTupleType | ParsedInterfaceType | ParsedReferenceType | ParsedUnionType | ParsedIntersectionType | ParsedImportType | ParsedSymbolType} ParsedType */
	/** @typedef {ParsedType | MergedClassType} MergedType */

	/**
	 * @param {ts.Symbol=} symbol the symbol
	 * @param {ts.Symbol=} aliasSymbol the aliasing symbol when set
	 * @param {string[]=} suffix additional suffix
	 * @returns {string[]} name of the symbol
	 */
	const parseName = (symbol, aliasSymbol, suffix = []) => {
		const getName = (symbol) => {
			if (!symbol) return undefined;
			if (symbol.getFlags() & ts.SymbolFlags.ValueModule) return sourceFile;
			const name = symbol.name;
			return name &&
				name !== ts.InternalSymbolName.Type &&
				name !== ts.InternalSymbolName.Object
				? toIdentifier(name)
				: undefined;
		};
		const valueDeclaration =
			getDeclaration(symbol) || getDeclaration(aliasSymbol) || undefined;
		const sourceFile =
			valueDeclaration &&
			toIdentifier(
				valueDeclaration
					.getSourceFile()
					.fileName.slice(rootPath.length + 1)
					.replace(/^.*\/(?!index)/, "")
					.replace(/^lib\./, "")
					.replace(/\.(js|(d\.)?ts)$/, "")
			);
		const syntaxType =
			valueDeclaration &&
			({
				[ts.SyntaxKind.ObjectLiteralExpression]: "Object",
				[ts.SyntaxKind.SourceFile]: "Module",
				[ts.SyntaxKind.JSDocTypedefTag]: "Object",
				[ts.SyntaxKind.JSDocTypeLiteral]: "Object",
				[ts.SyntaxKind.TypeAliasDeclaration]: "Alias",
				[ts.SyntaxKind.FunctionDeclaration]: "Function",
				[ts.SyntaxKind.FunctionExpression]: "Function",
				[ts.SyntaxKind.FunctionType]: "Function",
				[ts.SyntaxKind.JSDocSignature]: "Function",
				[ts.SyntaxKind.JSDocCallbackTag]: "Function",
				[ts.SyntaxKind.MethodDeclaration]: "Method",
				[ts.SyntaxKind.MethodSignature]: "Method",
				[ts.SyntaxKind.EnumDeclaration]: "Enum",
				[ts.SyntaxKind.ClassDeclaration]: "Class",
				[ts.SyntaxKind.ClassExpression]: "Class",
				[ts.SyntaxKind.InterfaceDeclaration]: "Interface",
			}[valueDeclaration.kind] ||
				`Kind${valueDeclaration.kind}_`);
		const name1 = getName(symbol);
		const name2 = getName(aliasSymbol);
		const result = [name1, name2, ...suffix, syntaxType, sourceFile];
		if (!name1 && !name2) {
			result.unshift(AnonymousType);
		}
		return Array.from(new Set(result.filter((x) => x !== undefined)));
	};

	/**
	 * @param {ts.Signature} signature signature
	 * @returns {ParsedSignature} parsed signature
	 */
	const parseSignature = (signature) => {
		let canBeOptional = true;
		return {
			signature,
			typeParameters:
				signature.typeParameters && signature.typeParameters.length > 0
					? signature.typeParameters
					: undefined,
			args: signature
				.getParameters()
				.slice()
				.reverse()
				.map((p) => {
					const valueDeclaration = p.valueDeclaration;
					const type = checker.getTypeOfSymbolAtLocation(p, valueDeclaration);
					const optional =
						((p.getFlags() & ts.SymbolFlags.Optional) !== 0 ||
							(type.getFlags() & ts.TypeFlags.Any) !== 0) &&
						canBeOptional;
					canBeOptional = canBeOptional && optional;
					return {
						name: p.name,
						optional,
						spread:
							valueDeclaration &&
							ts.isParameter(valueDeclaration) &&
							!!valueDeclaration.dotDotDotToken,
						type,
					};
				})
				.reverse(),
			returnType: signature.getReturnType(),
		};
	};

	/**
	 * @param {ts.Type} type type
	 * @returns {ParsedType | undefined} parsed type
	 */
	const parseType = (type) => {
		/**
		 * @param {ts.Symbol[]} symbols list of symbols
		 * @param {ts.Type[]=} baseTypes base types from which properties should be omitted
		 * @returns {PropertiesMap} map of types
		 */
		const toPropMap = (symbols, baseTypes = []) => {
			/** @type {PropertiesMap} */
			const properties = new Map();
			for (const prop of symbols) {
				const name = prop.name;
				if (name === "prototype") continue;
				if (name.startsWith("_")) continue;
				if (baseTypes.some((t) => t.getProperty(name))) continue;
				let modifierFlags;
				let innerType = prop.type;
				const decl = getDeclaration(prop);
				if (decl) {
					if (!innerType) {
						innerType = checker.getTypeOfSymbolAtLocation(prop, decl);
					}
					modifierFlags = ts.getCombinedModifierFlags(decl);
				}
				if (modifierFlags & ts.ModifierFlags.Private) {
					continue;
				}
				const flags = prop.getFlags();
				properties.set(name, {
					type: innerType,
					method: (flags & ts.SymbolFlags.Method) !== 0,
					optional: (flags & ts.SymbolFlags.Optional) !== 0,
					readonly:
						((flags & ts.SymbolFlags.GetAccessor) !== 0 &&
							(flags & ts.SymbolFlags.SetAccessor) === 0) ||
						(modifierFlags & ts.ModifierFlags.Readonly) !== 0,
				});
			}
			return properties;
		};

		if (/** @type {any} */ (type).isTypeParameter()) {
			return {
				type: "primitive",
				name: type.symbol.name,
			};
		}

		if (type.isUnion()) {
			return {
				type: "union",
				symbolName: parseName(type.symbol, type.aliasSymbol),
				types: type.types,
			};
		}

		if (type.isIntersection()) {
			return {
				type: "intersection",
				symbolName: parseName(type.symbol, type.aliasSymbol),
				types: type.types,
			};
		}

		const flags = type.getFlags();

		if (flags & ts.TypeFlags.Literal)
			return { type: "primitive", name: checker.typeToString(type) };
		if (flags & ts.TypeFlags.Any) return { type: "primitive", name: "any" };
		if (flags & ts.TypeFlags.Unknown)
			return { type: "primitive", name: "unknown" };
		if (flags & ts.TypeFlags.String)
			return { type: "primitive", name: "string" };
		if (flags & ts.TypeFlags.Number)
			return { type: "primitive", name: "number" };
		if (flags & ts.TypeFlags.Boolean)
			return { type: "primitive", name: "boolean" };
		if (flags & ts.TypeFlags.BigInt)
			return { type: "primitive", name: "bigint" };
		if (flags & ts.TypeFlags.Void) return { type: "primitive", name: "void" };
		if (flags & ts.TypeFlags.Undefined)
			return { type: "primitive", name: "undefined" };
		if (flags & ts.TypeFlags.Null) return { type: "primitive", name: "null" };
		if (flags & ts.TypeFlags.Never) return { type: "primitive", name: "never" };
		if (flags & ts.TypeFlags.Void) return { type: "primitive", name: "void" };

		const objectFlags =
			flags & ts.TypeFlags.Object
				? /** @type {ts.ObjectType} */ (type).objectFlags
				: 0;
		if (objectFlags & ts.ObjectFlags.JSLiteral) {
			return {
				type: "interface",
				symbolName: [AnonymousType, "Literal"],
				class: false,
				properties: toPropMap(type.getProperties()),
				constructors: [],
				calls: [],
				baseTypes: [],
			};
		}

		if (type.aliasSymbol) {
			const aliasType = checker.getDeclaredTypeOfSymbol(type.aliasSymbol);
			if (aliasType && aliasType !== type) {
				return {
					type: "reference",
					target: aliasType,
					typeArguments: type.aliasTypeArguments || [],
				};
			}
		}

		const symbol = type.aliasSymbol || type.getSymbol();

		if (objectFlags & ts.ObjectFlags.Reference) {
			const typeRef = /** @type {ts.TypeReference} */ (type);
			const typeArguments = checker.getTypeArguments(typeRef);
			if (objectFlags & ts.ObjectFlags.Tuple) {
				return {
					type: "primitive",
					name: "[]",
				};
			}
			if (typeRef !== typeRef.target) {
				return {
					type: "reference",
					target: typeRef.target,
					typeArguments,
				};
			}
		}

		if (symbol) {
			const decl = getDeclaration(symbol);
			if (
				decl &&
				decl.getSourceFile().isDeclarationFile &&
				decl
					.getSourceFile()
					.fileName.slice(rootPath.length + 1)
					.startsWith("node_modules/")
			) {
				const externalSource = decl.getSourceFile();
				const symbolToExport = getExportsOfSourceFile(externalSource);
				const exportName = symbolToExport.get(/** @type {any} */ (decl).symbol);
				if (exportName === undefined) {
					if (verbose) {
						console.log(
							`${parseName(/** @type {any} */ (decl).symbol).join(
								" "
							)} is an imported symbol, but couldn't find export in ${
								externalSource.fileName
							} (exports: ${[...symbolToExport.values()].sort().join(", ")})`
						);
					}
				} else {
					if (isSourceFileModule(externalSource)) {
						const match = /^(.+\/node_modules\/(?:@types\/)?)((?:@[^/]+\/)?[^/]+)(.*?)(\.d\.ts)?$/.exec(
							externalSource.fileName
						);
						if (!match) {
							console.error(
								`${externalSource.fileName} doesn't match node_modules import schema`
							);
						} else {
							let from = match[2] + match[3];
							try {
								const pkg = require(match[1] + match[2] + "/package.json");
								const regExp = new RegExp(
									"^(\\.\\/)?" + quoteMeta(match[3].slice(1)) + "(\\.d\\.ts)?$"
								);
								const types = pkg.types || "index.d.ts";
								if (regExp.test(types)) {
									from = match[2];
								}
							} catch (e) {
								// sorry, doesn't work
							}
							return {
								type: "import",
								symbolName: parseName(symbol, type.aliasSymbol, [
									toIdentifier(exportName),
									"Import",
								]),
								exportName,
								from,
							};
						}
					} else {
						const match = /"(.+?)"\.?(.*)$/.exec(exportName);
						if (match) {
							return {
								type: "import",
								symbolName: parseName(symbol, type.aliasSymbol, [
									toIdentifier(match[2]),
									"Import",
								]),
								exportName: match[2],
								from: match[1],
							};
						}
						return {
							type: "primitive",
							name: exportName,
						};
					}
				}
			}
		}

		const symbolName = parseName(symbol, type.aliasSymbol);

		if (flags & ts.TypeFlags.UniqueESSymbol) {
			return {
				type: "symbol",
				symbolName,
			};
		}

		return {
			type: "interface",
			symbolName,
			class: type.isClass(),
			properties: toPropMap(type.getProperties(), type.getBaseTypes()),
			constructors: type.getConstructSignatures().map(parseSignature),
			calls: type.getCallSignatures().map(parseSignature),
			numberIndex: type.getNumberIndexType(),
			stringIndex: type.getStringIndexType(),
			baseTypes: type.getBaseTypes() || [],
			typeParameters:
				type.isClassOrInterface() &&
				type.typeParameters &&
				type.typeParameters.length > 0
					? type.typeParameters
					: type.aliasSymbol &&
					  type.aliasTypeArguments &&
					  type.aliasTypeArguments.length > 0
					? type.aliasTypeArguments
					: undefined,
		};
	};

	/** @type {Map<ts.Type, MergedType>} */
	const parsedCollectedTypes = new Map();

	for (const type of collectedTypes) {
		const parsed = parseType(type);
		if (!parsed) {
			console.error(checker.typeToString(type), "can't be parsed");
			continue;
		}
		parsedCollectedTypes.set(type, parsed);
		switch (parsed.type) {
			case "union":
			case "intersection":
				for (const inner of parsed.types) captureType(type, inner);
				break;
			case "reference":
				captureType(type, parsed.target);
				for (const inner of parsed.typeArguments) captureType(type, inner);
				break;
			case "interface":
				for (const prop of parsed.baseTypes) captureType(type, prop);
				for (const prop of parsed.properties.values())
					captureType(type, prop.type);
				for (const call of parsed.calls) {
					for (const arg of call.args) {
						captureType(type, arg.type);
					}
					captureType(type, call.returnType);
				}
				for (const construct of parsed.constructors) {
					for (const arg of construct.args) {
						captureType(type, arg.type);
					}
					captureType(type, construct.returnType);
				}
				if (parsed.numberIndex) captureType(type, parsed.numberIndex);
				if (parsed.stringIndex) captureType(type, parsed.stringIndex);
				if (parsed.typeParameters)
					for (const prop of parsed.typeParameters) captureType(type, prop);
				break;
		}
	}

	const isSimpleFunction = (parsed) => {
		return (
			parsed.type === "interface" &&
			parsed.properties.size === 0 &&
			parsed.constructors.length === 0 &&
			parsed.calls.length === 1 &&
			(!parsed.typeParameters || parsed.typeParameters.length === 0)
		);
	};

	/// Merge interfaces to classes ///
	/**
	 * @param {ts.Type} type the type
	 * @returns {boolean} true, when it can be classified
	 */
	const canBeClassified = (type) => {
		const parsed = parsedCollectedTypes.get(type);
		if (parsed === undefined) return false;
		if (parsed.type === "class") return true;
		if (
			parsed.type !== "interface" ||
			parsed.calls.length !== 0 ||
			parsed.constructors.length !== 0 ||
			parsed.baseTypes.length > 1
		) {
			return false;
		}
		return (
			parsed.baseTypes.length === 0 || canBeClassified(parsed.baseTypes[0])
		);
	};

	const toBeClassified = new Set();

	for (const [type, parsed] of parsedCollectedTypes) {
		if (
			parsed.type === "interface" &&
			parsed.constructors.length > 0 &&
			!parsed.typeParameters &&
			parsed.calls.length === 0 &&
			!parsed.numberIndex &&
			!parsed.stringIndex &&
			parsed.baseTypes.length === 0
		) {
			const instanceType = parsed.constructors[0].returnType;
			if (parsed.constructors.every((c) => c.returnType === instanceType)) {
				const instance = parsedCollectedTypes.get(instanceType);
				if (
					instance !== undefined &&
					instance.type === "interface" &&
					instance.calls.length === 0 &&
					instance.constructors.length === 0 &&
					((instance.baseTypes.length === 1 &&
						canBeClassified(instance.baseTypes[0])) ||
						instance.baseTypes.length === 0)
				) {
					/** @type {Omit<MergedClassType, "type" | "correspondingType">} */
					const merged = {
						symbolName: instance.symbolName,
						properties: instance.properties,
						staticProperties: parsed.properties,
						constructors: parsed.constructors,
						numberIndex: instance.numberIndex,
						stringIndex: instance.stringIndex,
						typeParameters: instance.typeParameters,
						baseType: instance.baseTypes[0],
					};
					parsedCollectedTypes.set(instanceType, {
						type: "class",
						correspondingType: type,
						...merged,
					});
					parsedCollectedTypes.set(type, {
						type: "typeof class",
						correspondingType: instanceType,
						...merged,
					});
					if (merged.baseType) toBeClassified.add(merged.baseType);
				}
			}
		}
	}

	for (const type of toBeClassified) {
		const parsed = parsedCollectedTypes.get(type);
		if (!parsed) continue;
		if (parsed.type !== "interface") continue;
		/** @type {MergedClassType} */
		const newParsed = {
			type: "class",
			correspondingType: undefined,
			symbolName: parsed.symbolName,
			properties: parsed.properties,
			staticProperties: new Map(),
			constructors: [],
			numberIndex: parsed.numberIndex,
			stringIndex: parsed.stringIndex,
			typeParameters: parsed.typeParameters,
			baseType: parsed.baseTypes[0],
		};
		parsedCollectedTypes.set(type, newParsed);
	}

	/// Merge identical types ///

	/**
	 * @param {string} prefix type parameter prefix
	 * @param {ParsedSignature} signature signature
	 * @param {number} index signature index
	 * @returns {any[] | undefined} hash
	 */
	const getSigHash = (prefix, signature, index) => {
		const { args, returnType, typeParameters } = signature;
		const typeParametersMap = new Map(
			typeParameters && typeParameters.map((t, i) => [t, i])
		);
		return [
			"args",
			...flatten(
				args.map((arg) => [arg.name, arg.optional, arg.spread, arg.type])
			),
			"return",
			returnType,
			"typeParameters",
			typeParameters ? typeParameters.length : 0,
		].map((item) => {
			const x = typeParametersMap.get(item);
			return x === undefined ? item : `${prefix}${index}_${x}`;
		});
	};

	/**
	 * @param {MergedType} parsed type
	 * @returns {any[] | undefined} hash
	 */
	const getTypeHash = (parsed) => {
		switch (parsed.type) {
			case "interface": {
				const {
					symbolName,
					baseTypes,
					calls,
					constructors,
					properties,
					numberIndex,
					stringIndex,
					typeParameters,
				} = parsed;
				if (
					calls.length === 0 &&
					constructors.length === 0 &&
					properties.size === 0
				) {
					// need to have something unique
					return undefined;
				}
				const callHashes = calls.map(getSigHash.bind(null, "call"));
				if (callHashes.some((x) => !x)) return undefined;
				const constructorHashes = constructors.map(
					getSigHash.bind(null, "constructor")
				);
				if (constructorHashes.some((x) => !x)) return undefined;
				const typeParametersMap = new Map(
					typeParameters && typeParameters.map((t, i) => [t, i])
				);
				return [
					symbolName[0],
					"base",
					...baseTypes,
					"calls",
					...flatten(callHashes),
					"constructors",
					...flatten(constructorHashes),
					"properties",
					...flatten(
						Array.from(properties, ([name, { type, optional }]) => [
							name,
							type,
							optional,
						])
					),
					"numberIndex",
					numberIndex,
					"stringIndex",
					stringIndex,
					"typeParameters",
					typeParameters ? typeParameters.length : 0,
				].map((item) => {
					const x = typeParametersMap.get(item);
					return x === undefined ? item : x;
				});
			}
		}
		return undefined;
	};

	const knownTypes = new TupleMap();
	for (const [type, parsed] of parsedCollectedTypes) {
		const hash = getTypeHash(parsed);
		if (!hash) continue;
		const otherType = knownTypes.get(hash);
		if (otherType) {
			parsedCollectedTypes.set(type, {
				type: "reference",
				target: otherType,
				typeArguments: [],
			});
		} else {
			knownTypes.set(hash, type);
		}
	}

	/// Determine names for types ///

	/** @type {[ts.Type, {symbolName: SymbolName}][]} */
	const needName = [];
	for (const [type, parsed] of parsedCollectedTypes) {
		switch (parsed.type) {
			case "interface": {
				if (
					parsed.typeParameters ||
					parsed.baseTypes.length > 0 ||
					(parsed.symbolName[0] !== AnonymousType &&
						!isSimpleFunction(parsed) &&
						exposedType !== type)
				) {
					needName.push([type, parsed]);
				}
				break;
			}
			case "union":
			case "intersection": {
				if (parsed.symbolName[0] !== AnonymousType && exposedType !== type) {
					needName.push([type, parsed]);
				}
				break;
			}
			case "class":
			case "symbol":
			case "import": {
				needName.push([type, parsed]);
				break;
			}
		}
	}

	const usedNames = new Set();
	const nameToQueueEntry = new Map();
	const findName = (symbolName, requeueOnConflict = false) => {
		let name;
		for (let i = 1; i <= symbolName.length; i++) {
			name = joinIdentifer(symbolName.slice(0, i));
			if (!usedNames.has(name)) {
				usedNames.add(name);
				return name;
			}
			if (requeueOnConflict) {
				if (verbose) {
					console.log(
						`Naming conflict: ${name} can't be used for ${symbolName.join(" ")}`
					);
				}
				const item = nameToQueueEntry.get(name);
				if (item) {
					needName.push(item);
					nameToQueueEntry.set(name, undefined);
				}
			}
		}
		let i = 1;
		while (usedNames.has(`${name}_${i}`)) i++;
		usedNames.add(`${name}_${i}`);
		return `${name}_${i}`;
	};

	const typeToVariable = new Map();
	for (const entry of needName) {
		const [type, item] = entry;
		let { symbolName } = item;
		const name = findName(symbolName, true);
		typeToVariable.set(type, name);
		nameToQueueEntry.set(name, entry);
	}

	/// Determine code for types

	/** @type {Set<string>} */
	const declarations = new Set();
	/** @type {Map<string, string>} */
	const declarationKeys = new Map();
	/** @type {Map<string, Set<string>>} */
	const imports = new Map();
	/** @type {Set<string>} */
	const importDeclarations = new Set();
	/** @type {string[]} */
	const exports = [];
	const typeToCode = new TupleMap();

	/**
	 * @param {string} key key for sorting
	 * @param {string} text content
	 * @returns {void}
	 */
	const addDeclaration = (key, text) => {
		declarations.add(text);
		declarationKeys.set(text, key);
	};

	/**
	 * @param {string} exportName exported name
	 * @param {string} name local identifier name
	 * @param {string} from source file
	 * @returns {void}
	 */
	const addImport = (exportName, name, from) => {
		if (!exportName.includes(".")) {
			let set = imports.get(from);
			if (set === undefined) {
				imports.set(from, (set = new Set()));
			}
			if (exportName === name) {
				set.add(name);
			} else {
				set.add(`${exportName} as ${name}`);
			}
		} else {
			importDeclarations.add(
				`type ${name} = import(${JSON.stringify(from)}).${exportName};`
			);
		}
	};

	/**
	 * @param {ts.Symbol | ts.Signature | undefined} symbol symbol
	 * @returns {string} documentation comment
	 */
	const getDocumentation = (symbol) => {
		if (!symbol) return "";
		const comments = symbol.getDocumentationComment(checker);
		if (comments.length === 0) return "";
		return `\n/**\n * ${comments
			.map((c) => c.text)
			.join("\n")
			.replace(/\n/g, "\n * ")}\n */\n`;
	};

	/**
	 * @param {ParsedSignature} sig the signature
	 * @param {Set<ts.Type>} typeArgs type args specified in context
	 * @param {"arrow" | "constructor" | "class-constructor" | undefined} type type of generated code
	 * @returns {string} code
	 */
	const sigToString = (sig, typeArgs, type = undefined) => {
		const sigTypeArgs = sig.typeParameters
			? `<${sig.typeParameters.map((t) => getCode(t, typeArgs)).join(", ")}>`
			: "";
		const innerTypeArgs = new Set(typeArgs);
		if (sig.typeParameters) {
			for (const t of sig.typeParameters) innerTypeArgs.add(t);
		}
		const args = `(${sig.args
			.map(
				(arg) =>
					`${arg.spread ? "..." : ""}${arg.name}${
						arg.optional ? "?" : ""
					}: ${getCode(arg.type, innerTypeArgs)}`
			)
			.join(", ")})`;
		switch (type) {
			case "arrow":
				return `${sigTypeArgs}${args} => ${getCode(
					sig.returnType,
					innerTypeArgs
				)}`;
			case "class-constructor":
				return `constructor${args}`;
			case "constructor":
				return `new ${sigTypeArgs}${args}: ${getCode(
					sig.returnType,
					innerTypeArgs
				)}`;
			default: {
				return `${sigTypeArgs}${args}: ${getCode(
					sig.returnType,
					innerTypeArgs
				)}`;
			}
		}
	};

	/**
	 * @param {ts.Type} type the type
	 * @param {ParsedInterfaceType | MergedClassType} parsed parsed type
	 * @param {Set<ts.Type>} typeArgs type args specified in context
	 * @returns {string[]} code items for interface
	 */
	const getInterfaceItems = (type, parsed, typeArgs) => {
		const items = [];
		for (const construct of parsed.constructors) {
			items.push(
				`${getDocumentation(construct.signature)}${sigToString(
					construct,
					typeArgs,
					parsed.type === "interface" ? "constructor" : "class-constructor"
				)}`
			);
		}
		if (parsed.type === "interface") {
			for (const call of parsed.calls) {
				items.push(
					`${getDocumentation(call.signature)}${sigToString(call, typeArgs)}`
				);
			}
		}
		if (parsed.numberIndex) {
			items.push(`[index: number]: ${getCode(parsed.numberIndex, typeArgs)}`);
		}
		if (parsed.stringIndex) {
			items.push(`[index: string]: ${getCode(parsed.stringIndex, typeArgs)}`);
		}

		const handleProperties = (properties, prefix = "") => {
			for (const [name, { type: propType, optional, readonly }] of properties) {
				const code = getCode(propType, typeArgs);
				const p = prefix + (readonly ? "readonly " : "");
				if (code.startsWith("(undefined | ") && !optional) {
					items.push(
						`${getDocumentation(
							type.getProperty(name)
						)}${p}${name}?: (${code.slice("(undefined | ".length)}`
					);
				} else if (optional) {
					items.push(
						`${getDocumentation(type.getProperty(name))}${p}${name}?: ${code}`
					);
				} else {
					items.push(
						`${getDocumentation(type.getProperty(name))}${p}${name}: ${code}`
					);
				}
			}
		};

		handleProperties(parsed.properties);
		if (parsed.type === "class" && parsed.correspondingType) {
			handleProperties(parsed.staticProperties, "static ");
		}
		return items;
	};

	/**
	 * @param {ts.Type} type the type
	 * @param {Set<ts.Type>} typeArgs type args specified in context
	 * @param {boolean} hasTypeArgs true, if it has type args specified
	 * @returns {string} code
	 */
	const getCodeInternal = (type, typeArgs, hasTypeArgs) => {
		const parsed = parsedCollectedTypes.get(type);
		if (!parsed) return "unknown /* no parsed data */";
		switch (parsed.type) {
			case "primitive":
				return parsed.name;
			case "intersection":
				return `(${parsed.types.map((t) => getCode(t, typeArgs)).join(" & ")})`;
			case "union":
				return `(${parsed.types
					.map((t) => getCode(t, typeArgs))
					.join(" | ")})`.replace(
					/(^\(|\| )false \| true(\)$| \|)/g,
					"$1boolean$2"
				);
			case "reference": {
				if (parsed.typeArguments.length === 0)
					return getCode(parsed.target, typeArgs, hasTypeArgs);
				const parsedTarget = parsedCollectedTypes.get(parsed.target);
				if (
					parsedTarget &&
					parsedTarget.type === "primitive" &&
					parsedTarget.name === "[]"
				) {
					return `[${parsed.typeArguments
						.map((t) => getCode(t, typeArgs))
						.join(", ")}]`;
				}
				return `${getCode(
					parsed.target,
					typeArgs,
					true
				)}<${parsed.typeArguments
					.map((t) => getCode(t, typeArgs))
					.join(", ")}>`;
			}
			case "interface": {
				const variable = typeToVariable.get(type);
				if (variable !== undefined) {
					if (
						!hasTypeArgs &&
						parsed.typeParameters &&
						/*parsed.typeParameters.some(t => typeArgs.has(t))*/ true
					) {
						return `${variable}<${parsed.typeParameters.map((t) =>
							getCode(t, typeArgs)
						)}>`;
					}
					return variable;
				}
				if (isSimpleFunction(parsed)) {
					return `(${sigToString(parsed.calls[0], typeArgs, "arrow")})`;
				}
				return `{ ${getInterfaceItems(type, parsed, typeArgs).join("; ")} }`;
			}
			case "class": {
				const variable = typeToVariable.get(type);
				if (
					!hasTypeArgs &&
					parsed.typeParameters &&
					/*parsed.typeParameters.some(t => typeArgs.has(t))*/ true
				) {
					return `${variable}<${parsed.typeParameters.map((t) =>
						getCode(t, typeArgs)
					)}>`;
				}
				return variable;
			}
			case "typeof class": {
				const variable = typeToVariable.get(parsed.correspondingType);
				return `typeof ${variable}`;
			}
			case "symbol": {
				const variable = typeToVariable.get(type);
				return `typeof ${variable}`;
			}
			default: {
				const variable = typeToVariable.get(type);
				if (variable !== undefined) {
					return variable;
				}
			}
		}
		return `unknown /* failed to generate code: ${parsed.type} */`;
	};

	let codeGenerationContext = "";

	const unusedTempNames = new TupleMap();

	/**
	 * @param {ts.Type} type the type
	 * @param {Set<ts.Type>} typeArgs type args specified in context
	 * @param {boolean} hasTypeArgs true, if it has type args specified
	 * @returns {string} code
	 */
	const getCode = (type, typeArgs, hasTypeArgs = false) => {
		const tuple = [type, ...typeArgs, hasTypeArgs];
		const code = typeToCode.get(tuple);
		if (code !== undefined) {
			unusedTempNames.delete(tuple);
			return code;
		}
		const parsed = parsedCollectedTypes.get(type);
		const tempName = findName(
			(parsed && "symbolName" in parsed && parsed.symbolName) || [
				codeGenerationContext + "AnonymousCircularType",
			]
		);
		typeToCode.set(tuple, tempName);
		unusedTempNames.add(tuple);
		const newCode = getCodeInternal(type, typeArgs, hasTypeArgs);
		const used = !unusedTempNames.has(tuple);
		unusedTempNames.delete(tuple);
		if (used) {
			addDeclaration(tempName, `type ${tempName} = ${newCode};`);
		} else {
			usedNames.delete(tempName);
			typeToCode.set(tuple, newCode);
		}
		return newCode;
	};

	for (const [type, parsed] of parsedCollectedTypes) {
		switch (parsed.type) {
			case "interface": {
				const name = typeToVariable.get(type);
				if (name) {
					codeGenerationContext = name;
					const typeArgs = new Set(parsed.typeParameters);
					addDeclaration(
						name,
						`${getDocumentation(type.getSymbol())}interface ${name}${
							parsed.typeParameters
								? `<${parsed.typeParameters
										.map((t) => getCode(t, new Set()))
										.join(", ")}>`
								: ""
						}${
							parsed.baseTypes.length > 0
								? ` extends ${parsed.baseTypes
										.map((t) => getCode(t, typeArgs))
										.join(", ")}`
								: ""
						} {\n${getInterfaceItems(type, parsed, typeArgs)
							.map((i) => `\t${i};`)
							.join("\n")}\n}`
					);
					codeGenerationContext = "";
				}
				break;
			}
			case "class": {
				const name = typeToVariable.get(type);
				if (name) {
					codeGenerationContext = name;
					const typeArgs = new Set(parsed.typeParameters);
					addDeclaration(
						name,
						`declare ${
							parsed.constructors.length === 0 ? "abstract class" : "class"
						} ${name}${
							parsed.typeParameters
								? `<${parsed.typeParameters
										.map((t) => getCode(t, new Set()))
										.join(", ")}>`
								: ""
						}${
							parsed.baseType
								? ` extends ${getCode(parsed.baseType, typeArgs)}`
								: ""
						} {\n${getInterfaceItems(type, parsed, typeArgs)
							.map((i) => `\t${i};`)
							.join("\n")}\n}`
					);
					codeGenerationContext = "";
				}
				break;
			}
			case "union":
			case "intersection":
				const name = typeToVariable.get(type);
				if (name) {
					addDeclaration(
						name,
						`type ${name} = ${parsed.types
							.map((t) => getCode(t, new Set()))
							.join(parsed.type === "intersection" ? " & " : " | ")
							.replace(/(^|\| )false \| true($| \|)/g, "$1boolean$2")};`
					);
				}
				break;
			case "symbol": {
				const name = typeToVariable.get(type);
				addDeclaration(name, `declare const ${name}: unique symbol;`);
				break;
			}
			case "import": {
				const name = typeToVariable.get(type);
				addImport(parsed.exportName, name, parsed.from);
				break;
			}
		}
	}

	if (exposedType) {
		const exportsName = findName(["exports"]);
		exports.push(
			`declare const ${exportsName}: ${getCode(exposedType, new Set())};`
		);
		exports.push(`export = ${exportsName};`);
	}

	const outputFilePath = path.resolve(root, outputFile);

	const sortedDeclarations = [...declarations].sort((a, b) => {
		const ak = /** @type {string} */ (declarationKeys.get(a));
		const bk = /** @type {string} */ (declarationKeys.get(b));
		if (ak < bk) return -1;
		if (ak > bk) return 1;
		return 0;
	});

	let source = [
		"/**",
		" * This file was automatically generated.",
		" * DO NOT MODIFY BY HAND.",
		" * Run `yarn special-lint-fix` to update",
		" */",
		"",
		...[...imports.keys()]
			.sort()
			.map(
				(from) =>
					`import { ${[...imports.get(from)]
						.sort()
						.join(", ")} } from ${JSON.stringify(from)}`
			),
		...[...importDeclarations].sort(),
		"",
		...sortedDeclarations,
		"",
		...exports,
	].join("\n");
	try {
		const prettierOptions = await prettier.resolveConfig(outputFilePath);
		if (!prettierOptions) {
			console.error("Prettier options not found");
			return;
		}

		source = prettier.format(source, prettierOptions);
	} catch (e) {
		exitCode = 1;
		console.error(e.message);
	}

	let needUpdate = true;
	try {
		if ((await fs.readFile(outputFilePath, "utf-8")) === source) {
			needUpdate = false;
		}
	} catch (e) {
		// ignore
	}

	if (needUpdate) {
		if (doWrite) {
			await fs.writeFile(outputFilePath, source);
		} else {
			exitCode = 1;
			console.error("types.d.ts need to be updated.");
			console.error("run 'yarn special-lint-fix' to update.");
		}
	}

	// const hint = typeNameHints.get(type);
	// if (!hint) continue;
	// const name = `${hint.source.fileName.replace(/[^A-Za-z]+/g, "_")}_${
	// hint.symbol ? hint.symbol.getName().replace(/[^A-Za-z]+/g, "_") : ""
	// }_${hint.name || uniqueId++}`;

	process.exitCode = exitCode;
})();
