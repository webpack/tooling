const argv = require("../lib/argv");

const { write: doWrite, root, schemas: schemasGlob } = argv;

const fs = require("fs");
const path = require("path");
const glob = require("glob");
const { pathToFileURL, fileURLToPath } = require("url");
const processSchema = require("../lib/process-schema");
const terser = require("terser");
const { default: Ajv, _, Name } = require("ajv");
const standaloneCode = require("ajv/dist/standalone").default;

const ajv = new Ajv({
	code: { source: true, optimize: true },
	messages: false,
	logger: false,
	loadSchema: (uri) => {
		schemaPath = fileURLToPath(uri);
		const schema = require(schemaPath);
		const processedSchema = processJson(schema);
		processedSchema.$id = uri;
		return processedSchema;
	},
});

ajv.addKeyword({
	keyword: "instanceof",
	schemaType: "string",
	code(ctx) {
		const { data, schema } = ctx;
		ctx.fail(_`!(${data} instanceof ${new Name(schema)})`);
	},
});

ajv.addKeyword({
	keyword: "absolutePath",
	type: "string",
	schemaType: "boolean",

	code(ctx) {
		const { data, schema } = ctx;
		ctx.fail(
			_`${data}.includes("!") || (absolutePathRegExp.test(${data}) !== ${schema})`
		);
	},
});

ajv.removeKeyword("minLength");
ajv.addKeyword({
	keyword: "minLength",
	type: "string",
	schemaType: "number",

	code(ctx) {
		const { data, schema } = ctx;
		if (schema !== 1)
			throw new Error("Schema precompilation only supports minLength: 1");
		ctx.fail(_`${data}.length < 1`);
	},
});

ajv.removeKeyword("enum");
ajv.addKeyword({
	keyword: "enum",
	schemaType: "array",
	$data: true,

	code(ctx) {
		const { data, schema } = ctx;
		for (const item of schema) {
			if (typeof item === "object" && item !== null) {
				throw new Error(
					`Schema precompilation only supports primitive values in enum: ${JSON.stringify(
						item,
						null,
						2
					)}`
				);
			}
		}
		ctx.fail(
			schema.map((x) => _`${data} !== ${x}`).reduce((a, b) => _`${a} && ${b}`)
		);
	},
});

const schemas = glob.sync(schemasGlob, { cwd: root, absolute: true });

const EXCLUDED_PROPERTIES = [
	"title",
	"description",
	"cli",
	"implements",
	"tsType",
];

const processJson = processSchema.bind(null, {
	schema: (json) => {
		for (const p of EXCLUDED_PROPERTIES) {
			delete json[p];
		}
		return json;
	},
});

const postprocess = async (code) => {
	// add hoisted values
	if (/absolutePathRegExp/.test(code))
		code = `const absolutePathRegExp = /^(?:[A-Za-z]:[\\\\/]|\\\\\\\\|\\/)/;${code}`;

	// remove unneccessary error code:
	code = code.replace(/\{instancePath[^{}]+,keyword:[^{}]+,/g, "{");

	// minimize
	code = (
		await terser.minify(code, {
			compress: {
				passes: 3,
			},
			mangle: {
				properties: true,
			},
			ecma: 2015,
			toplevel: true,
		})
	).code;
	return code;
};

const precompileSchema = async (schemaPath) => {
	if (path.basename(schemaPath).startsWith("_")) return;
	try {
		const schema = require(schemaPath);
		const processedSchema = processJson(schema);
		processedSchema.$id = pathToFileURL(schemaPath).href;
		const validate = await ajv.compileAsync(processedSchema);
		const code = await postprocess(standaloneCode(ajv, validate));
		const precompiledSchemaPath = schemaPath.replace(/\.json$/, ".check.js");
		let normalizedContent = "";
		try {
			const content = fs.readFileSync(precompiledSchemaPath, "utf-8");
			normalizedContent = content.replace(/\r\n?/g, "\n");
		} catch (e) {
			// ignore
		}
		if (normalizedContent.trim() !== code.trim()) {
			if (doWrite) {
				fs.writeFileSync(precompiledSchemaPath, code, "utf-8");
				console.error(`${precompiledSchemaPath} updated`);
			} else {
				console.error(`${precompiledSchemaPath} need to be updated`);
				process.exitCode = 1;
			}
		}
	} catch (e) {
		e.message += "\nduring precompilation of " + schemaPath;
		throw e;
	}
};

(async () => {
	for (let absPath of schemas) {
		precompileSchema(absPath);
	}
})().catch((e) => {
	console.error(e.stack);
	process.exitCode = 1;
});
