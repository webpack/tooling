const NESTED_WITH_NAME = ["definitions", "properties"];

const NESTED_DIRECT = ["items", "additionalProperties", "not"];

const NESTED_ARRAY = ["oneOf", "anyOf", "allOf"];

const processSchema = (visitor, json, context) => {
	if (visitor.schema) json = visitor.schema(json, context);

	for (const name of NESTED_WITH_NAME) {
		if (name in json && json[name] && typeof json[name] === "object") {
			if (visitor.object) json[name] = visitor.object(json[name], context);
			for (const key in json[name]) {
				json[name][key] = processSchema(visitor, json[name][key], context);
			}
		}
	}
	for (const name of NESTED_DIRECT) {
		if (name in json && json[name] && typeof json[name] === "object") {
			json[name] = processSchema(visitor, json[name], context);
		}
	}
	for (const name of NESTED_ARRAY) {
		if (name in json && Array.isArray(json[name])) {
			for (let i = 0; i < json[name].length; i++) {
				json[name][i] = processSchema(visitor, json[name][i], context);
			}
			if (visitor.array) visitor.array(json[name], context);
		}
	}

	return json;
};
module.exports = processSchema;
