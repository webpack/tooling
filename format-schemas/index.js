const argv = require("../lib/argv");

const { write: doWrite, root, schemas: schemasGlob } = argv;

const fs = require("fs");
const glob = require("glob");
const prettier = require("prettier");
const processSchema = require("../lib/process-schema");

const schemas = glob.sync(schemasGlob, { cwd: root, absolute: true });
const minSlashes = schemas
	.map((p) => p.split(/[\\/]/).length)
	.reduce((a, b) => Math.min(a, b), Infinity);
const baseSchemaPaths = schemas.filter(
	(p) => p.split(/[\\/]/).length === minSlashes
);
const baseDefinitions = new Map();
for (const baseSchemaPath of baseSchemaPaths) {
	for (const [name, schema] of Object.entries(
		require(baseSchemaPath).definitions
	)) {
		baseDefinitions.set(name, schema);
	}
}

const sortObjectAlphabetically = (obj) => {
	const keys = Object.keys(obj).sort();
	const newObj = {};
	for (const key of keys) {
		newObj[key] = obj[key];
	}
	return newObj;
};

const typeOrder = [
	"array",
	"enum",
	"RegExp",
	"number",
	"boolean",
	"string",
	"object",
	"Function",
	undefined,
];

const sortArrayByType = (array) => {
	array.sort((a, b) => {
		const aType = a.type || a.instanceof || (a.enum && "enum");
		const bType = b.type || b.instanceof || (b.enum && "enum");
		const aPos = typeOrder.indexOf(aType);
		const bPos = typeOrder.indexOf(bType);
		if (aPos === bPos) {
			return array.indexOf(a) - array.indexOf(b);
		}
		return aPos - bPos;
	});
};

const sortObjectWithList = (obj, props) => {
	const keys = Object.keys(obj)
		.filter((p) => !props.includes(p))
		.sort();
	const newObj = {};
	for (const key of props) {
		if (key in obj) {
			newObj[key] = obj[key];
		}
	}
	for (const key of keys) {
		newObj[key] = obj[key];
	}
	return newObj;
};

const PROPERTIES = [
	"$ref",
	"definitions",

	"$id",
	"id",
	"title",
	"description",
	"type",

	"cli",

	"items",
	"minItems",
	"uniqueItems",

	"implements",
	"additionalProperties",
	"properties",
	"required",
	"minProperties",

	"oneOf",
	"anyOf",
	"allOf",
	"enum",

	"absolutePath",
	"minLength",

	"minimum",

	"instanceof",

	"tsType",
];

const processJson = processSchema.bind(null, {
	schema: (json, context) => {
		json = sortObjectWithList(json, PROPERTIES);

		if (json.definitions) {
			json.definitions = { ...json.definitions };
			for (const key of Object.keys(json.definitions)) {
				let baseDef = baseDefinitions.get(key);
				if (baseDef) {
					baseDef = processSchema(
						{
							schema: (json) => {
								const tsType = json.tsType;
								if (tsType) {
									json = {
										...json,
										tsType: tsType.replace(
											/\.\.\//g,
											context.importPrefix + "../"
										),
									};
								}
								return json;
							},
						},
						baseDef,
						{}
					);
					json.definitions[key] = baseDef;
				}
			}
		}

		if (json.implements) {
			const prefix = "#/definitions/";
			for (const impl of [].concat(json.implements)) {
				if (!impl.startsWith(prefix)) {
					console.warn(
						`"implements": "${impl}" -> should start with "${prefix}"`
					);
					continue;
				}
				const name = impl.slice(prefix.length);
				const referencedSchema = context.definitions[name];
				if (!referencedSchema) {
					console.warn(
						`"implements": "${impl}" -> referenced schema not found`
					);
					continue;
				}
				if (typeof referencedSchema.properties !== "object") {
					console.warn(
						`"implements": "${impl}" -> referenced schema has no properties`
					);
					continue;
				}
				json.properties = {
					...json.properties,
					...referencedSchema.properties,
				};
			}
		}

		return json;
	},
	array: (json, context) => {
		return sortArrayByType(json);
	},
	object: (json, context) => {
		return sortObjectAlphabetically(json);
	},
});

const formatSchema = (schemaPath) => {
	const json = require(schemaPath);
	const processedJson = processJson(json, {
		schema: json,
		definitions: json.definitions,
		importPrefix: "../".repeat(schemaPath.split(/[\\/]/).length - minSlashes),
	});
	const rawString = JSON.stringify(processedJson, null, 2);
	const config = prettier.resolveConfig.sync(schemaPath);
	const prettyString = prettier.format(rawString, config);
	let normalizedContent = "";
	try {
		const content = fs.readFileSync(schemaPath, "utf-8");
		normalizedContent = content.replace(/\r\n?/g, "\n");
	} catch (e) {
		// ignore
	}
	if (normalizedContent.trim() !== prettyString.trim()) {
		if (doWrite) {
			fs.writeFileSync(schemaPath, prettyString, "utf-8");
			console.error(`${schemaPath} updated`);
		} else {
			console.error(`${schemaPath} need to be updated`);
			process.exitCode = 1;
		}
	}
};

for (let absPath of schemas) {
	formatSchema(absPath);
}
