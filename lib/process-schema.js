/**
 * @typedef {import("json-schema").JSONSchema} JSONSchema
 */

/**
 * @typedef {Object.<string, JSONSchema | boolean>} ProcessContext
 */

/**
 * @typedef {Object} SchemaVisitor
 * @property {(schema: JSONSchema | boolean, context?: ProcessContext) => JSONSchema | boolean | void} [schema]
 * @property {(obj: JSONSchema | boolean, context?: ProcessContext) => JSONSchema | boolean | void} [object]
 * @property {(arr: (JSONSchema | boolean)[], context?: ProcessContext) => void} [array]
 */

const NESTED_WITH_NAME = ["definitions", "properties"];
const NESTED_DIRECT = ["items", "additionalProperties", "not"];
const NESTED_ARRAY = ["oneOf", "anyOf", "allOf"];

/**
 * Recursively processes a JSON Schema using a visitor pattern.
 *
 * @param {SchemaVisitor} visitor
 * @param {JSONSchema | boolean} json
 * @param {ProcessContext} [context]
 * @returns {JSONSchema | boolean}
 */
const processSchema = (visitor, json, context) => {
	if (!json || typeof json !== "object") return json;

	json = { ...json };

	if (typeof visitor?.schema === "function") {
		json = visitor.schema(json, context) || json;
	}

	for (const name of NESTED_WITH_NAME) {
		if (name in json && json[name] && typeof json[name] === "object" && !Array.isArray(json[name])) {
			if (typeof visitor?.object === "function") {
				json[name] = visitor.object(json[name], context) || json[name];
			}

			for (const key of Object.keys(json[name])) {
				json[name][key] = processSchema(visitor, json[name][key], context);
			}
		}
	}

	for (const name of NESTED_DIRECT) {
		if (name in json && json[name] && typeof json[name] === "object" && !Array.isArray(json[name])) {
			json[name] = processSchema(visitor, json[name], context);
		}
	}

	for (const name of NESTED_ARRAY) {
		if (name in json && Array.isArray(json[name])) {
			json[name] = json[name].map((item) => processSchema(visitor, item, context));

			if (typeof visitor?.array === "function") {
				visitor.array(json[name], context);
			}
		}
	}

	return json;
};

module.exports = processSchema;
