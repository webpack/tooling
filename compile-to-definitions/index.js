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

const prettierConfig = prettier.resolveConfig.sync(
	path.resolve(root, outputFolder, "result.d.ts")
);
const style = {
	printWidth: prettierConfig.printWidth,
	useTabs: prettierConfig.useTabs,
	tabWidth: prettierConfig.tabWidth,
};

const makeSchemas = () => {
	const schemas = glob.sync(schemasGlob, { cwd: root, absolute: true });
	const commonDir = path.resolve(findCommonDir(schemas));
	for (const absPath of schemas) {
		makeDefinitionsForSchema(absPath, commonDir);
	}
};

const makeDefinitionsForSchema = (absSchemaPath, schemasDir) => {
	const relPath = path.relative(schemasDir, absSchemaPath);
	const directory = path.dirname(relPath);
	const basename = path.basename(relPath, path.extname(relPath));
	const filename = path.resolve(
		root,
		outputFolder,
		`${path.join(directory, basename)}.d.ts`
	);
	const schema = JSON.parse(fs.readFileSync(absSchemaPath, "utf-8"));
	preprocessSchema(schema);
	compile(schema, basename, {
		bannerComment:
			"/*\n * This file was automatically generated.\n * DO NOT MODIFY BY HAND.\n * Run `yarn special-lint-fix` to update\n */",
		unreachableDefinitions: true,
		style,
	}).then(
		(ts) => {
			ts = ts.replace(
				/\s+\*\s+\* This interface was referenced by `.+`'s JSON-Schema\s+\* via the `definition` ".+"\./g,
				""
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
						`declarations/${relPath.replace(/\\/g, "/")}.d.ts updated`
					);
				} else {
					console.error(
						`declarations/${relPath.replace(
							/\\/g,
							"/"
						)}.d.ts need to be updated`
					);
					process.exitCode = 1;
				}
			}
		},
		(err) => {
			console.error(err);
			process.exitCode = 1;
		}
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

const preprocessSchema = (schema, root = schema) => {
	if ("definitions" in schema) {
		for (const key of Object.keys(schema.definitions)) {
			preprocessSchema(schema.definitions[key], root);
		}
	}
	if ("properties" in schema) {
		for (const key of Object.keys(schema.properties)) {
			const property = schema.properties[key];
			if ("$ref" in property) {
				const result = resolvePath(root, property.$ref);
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
				preprocessSchema(schema.properties[key], root);
			} else {
				preprocessSchema(property, root);
			}
		}
	}
	if ("items" in schema) {
		preprocessSchema(schema.items, root);
	}
	if (typeof schema.additionalProperties === "object") {
		preprocessSchema(schema.additionalProperties, root);
	}
	const arrayProperties = ["oneOf", "anyOf", "allOf"];
	for (const prop of arrayProperties) {
		if (Array.isArray(schema[prop])) {
			for (const item of schema[prop]) {
				preprocessSchema(item, root);
			}
		}
	}
	if ("type" in schema && schema.type === "array") {
		// Workaround for a typescript bug that
		// string[] is not assignable to [string, ...string]
		delete schema.minItems;
	}
};

makeSchemas();
