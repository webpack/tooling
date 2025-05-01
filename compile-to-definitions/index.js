const argv = require("../lib/argv");

const {
	write: doWrite,
	root,
	declarations: outputFolder,
	schemas: schemasGlob,
} = argv;

const fs = require("fs");
const path = require("path");
const prettier = require("prettier");
const glob = require("glob");
const findCommonDir = require("commondir");
const { compile } = require("json-schema-to-typescript");

const makeSchemas = () => {
	const schemas = glob.sync(schemasGlob, { cwd: root, absolute: true });
	const commonDir = path.resolve(findCommonDir(schemas));
	for (const absPath of schemas) {
		makeDefinitionsForSchema(absPath, commonDir);
	}
};

const makeDefinitionsForSchema = async (absSchemaPath, schemasDir) => {
	if (path.basename(absSchemaPath).startsWith("_")) return;
	const relPath = path.relative(schemasDir, absSchemaPath);
	const directory = path.dirname(relPath);
	const basename = path.basename(relPath, path.extname(relPath));
	const filename = path.resolve(
		root,
		outputFolder,
		`${path.join(directory, basename)}.d.ts`,
	);
	const schema = JSON.parse(fs.readFileSync(absSchemaPath, "utf-8"));
	const keys = Object.keys(schema);
	if (keys.length === 1 && keys[0] === "$ref") return;

	const prettierConfig = await prettier.resolveConfig(
		path.resolve(root, outputFolder, "result.d.ts"),
	);
	const style = {
		printWidth: prettierConfig.printWidth,
		useTabs: prettierConfig.useTabs,
		tabWidth: prettierConfig.tabWidth,
	};

	preprocessSchema(schema);
	compile(schema, basename, {
		bannerComment:
			"/*\n * This file was automatically generated.\n * DO NOT MODIFY BY HAND.\n * Run `yarn fix:special` to update\n */",
		unreachableDefinitions: true,
		unknownAny: false,
		style,
	}).then(
		(ts) => {
			ts = ts.replace(
				/\s+\*\s+\* This interface was referenced by `.+`'s JSON-Schema\s+\* via the `definition` ".+"\./g,
				"",
			);
			let normalizedContent = "";
			try {
				const content = fs.readFileSync(filename, "utf-8");
				normalizedContent = content.replace(/\r\n?/g, "\n");
			} catch (e) {
				// ignore
			}
			if (normalizedContent.trim() !== ts.trim()) {
				if (doWrite) {
					fs.mkdirSync(path.dirname(filename), { recursive: true });
					fs.writeFileSync(filename, ts, "utf-8");
					console.error(
						`declarations/${relPath.replace(/\\/g, "/")}.d.ts updated`,
					);
				} else {
					console.error(
						`declarations/${relPath.replace(
							/\\/g,
							"/",
						)}.d.ts need to be updated`,
					);
					process.exitCode = 1;
				}
			}
		},
		(err) => {
			console.error(err);
			process.exitCode = 1;
		},
	);
};

const resolvePath = (root, ref) => {
	const parts = ref.split("/");
	if (parts[0] !== "#") throw new Error("Unexpected ref");
	let current = root;
	for (const p of parts.slice(1)) {
		current = current[p];
	}
	return current;
};

const preprocessSchema = (schema, root = schema, path = []) => {
	if ("definitions" in schema) {
		for (const key of Object.keys(schema.definitions)) {
			preprocessSchema(schema.definitions[key], root, [key]);
		}
	}
	if ("properties" in schema) {
		for (const key of Object.keys(schema.properties)) {
			const property = schema.properties[key];
			if ("$ref" in property) {
				const result = resolvePath(root, property.$ref);
				if (!result) {
					throw new Error(
						`Unable to resolve "$ref": "${property.$ref}" in ${path.join("/")}`,
					);
				}
				schema.properties[key] = {
					description: result.description,
					anyOf: [property],
				};
			} else if (
				"oneOf" in property &&
				property.oneOf.length === 1 &&
				"$ref" in property.oneOf[0]
			) {
				const result = resolvePath(root, property.oneOf[0].$ref);
				schema.properties[key] = {
					description: property.description || result.description,
					anyOf: property.oneOf,
				};
				preprocessSchema(schema.properties[key], root, [...path, key]);
			} else {
				preprocessSchema(property, root, [...path, key]);
			}
		}
	}
	if ("items" in schema) {
		preprocessSchema(schema.items, root, [...path, "item"]);
	}
	if (typeof schema.additionalProperties === "object") {
		preprocessSchema(schema.additionalProperties, root, [...path, "property"]);
	}
	const arrayProperties = ["oneOf", "anyOf", "allOf"];
	for (const prop of arrayProperties) {
		if (Array.isArray(schema[prop])) {
			let i = 0;
			for (const item of schema[prop]) {
				preprocessSchema(item, root, [...path, item.type || i++]);
			}
		}
	}
	if ("type" in schema && schema.type === "array") {
		// Workaround for a typescript bug that
		// string[] is not assignable to [string, ...string]
		delete schema.minItems;
	}
	if ("implements" in schema) {
		const implementedProps = new Set();
		const implementedNames = [];
		for (const impl of [].concat(schema.implements)) {
			const referencedSchema = resolvePath(root, impl);
			for (const prop of Object.keys(referencedSchema.properties)) {
				implementedProps.add(prop);
			}
			implementedNames.push(/\/([^\/]+)$/.exec(impl)[1]);
		}
		const propEntries = Object.entries(schema.properties).filter(
			([name]) => !implementedProps.has(name),
		);
		if (propEntries.length > 0) {
			const key =
				path.map((x) => x[0].toUpperCase() + x.slice(1)).join("") + "Extra";
			implementedNames.push(key);
			const { implements, ...remainingSchema } = schema;
			root.definitions[key] = {
				...remainingSchema,
				properties: Object.fromEntries(propEntries),
			};
			preprocessSchema(root.definitions[key], root, [key]);
		}
		schema.tsType = implementedNames.join(" & ");
		return;
	}
	if (
		"properties" in schema &&
		typeof schema.additionalProperties === "object" &&
		!schema.tsType
	) {
		const { properties, additionalProperties, ...remaining } = schema;
		const key1 =
			path.map((x) => x[0].toUpperCase() + x.slice(1)).join("") + "Known";
		const key2 =
			path.map((x) => x[0].toUpperCase() + x.slice(1)).join("") + "Unknown";
		root.definitions[key1] = {
			...remaining,
			properties,
			additionalProperties: false,
		};
		preprocessSchema(root.definitions[key1], root, [key1]);
		root.definitions[key2] = {
			...remaining,
			additionalProperties,
		};
		preprocessSchema(root.definitions[key2], root, [key2]);
		schema.tsType = `${key1} & ${key2}`;
		return;
	}
};

makeSchemas();
