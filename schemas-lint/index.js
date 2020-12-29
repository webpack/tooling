"use strict";

const { root, schemas: schemasGlob } = require("../lib/argv");

const fs = require("fs");
const path = require("path");
const glob = require("glob");

const schemas = glob.sync(schemasGlob, { cwd: root });

for (const filename of schemas) {
	let content;

	try {
		const fileContent = fs.readFileSync(path.resolve(root, filename), "utf-8");
		content = JSON.parse(fileContent);
	} catch (e) {
		console.log(`${filename} can't be parsed: ${e}`);
		process.exitCode = 1;
	}

	if (content) {
		const arrayProperties = ["oneOf", "anyOf", "allOf"];
		const allowedProperties = [
			"definitions",
			"$ref",
			"$id",
			"title",
			"cli",
			"items",
			"implements",
			"properties",
			"additionalProperties",
			"type",
			"oneOf",
			"anyOf",
			"absolutePath",
			"description",
			"enum",
			"minLength",
			"pattern",
			"minimum",
			"maximum",
			"required",
			"uniqueItems",
			"minItems",
			"minProperties",
			"instanceof",
			"tsType",
			"not",
		];

		const isReference = (schema) => {
			return (
				"$ref" in schema ||
				("oneOf" in schema &&
					schema.oneOf.length === 1 &&
					"$ref" in schema.oneOf[0])
			);
		};

		const validateProperty = (path, property) => {
			if (isReference(property)) return;
			if (
				typeof property.description !== "string" ||
				property.description.length < 1
			) {
				console.log(`${path} should have a description set.`);
				process.exitCode = 1;
			} else if (!/^[A-Z`].*[^\.]\.$/.test(property.description)) {
				console.log(
					`${path}.description should start with an uppercase letter and end with a single dot.`
				);
				process.exitCode = 1;
			}
		};

		const walker = (path, item) => {
			const otherProperties = Object.keys(item).filter(
				(p) => allowedProperties.indexOf(p) < 0
			);
			if (otherProperties.length > 0) {
				console.log(
					`${path} should not have the ${
						otherProperties.length > 1 ? "properties" : "property"
					} ${otherProperties.join(", ")}`
				);
				process.exitCode = 1;
				// When allowing more properties make sure to add nice error messages for them in WebpackOptionsValidationError
			}

			if ("$ref" in item) {
				const otherProperties = Object.keys(item).filter((p) => p !== "$ref");
				if (otherProperties.length > 0) {
					console.log(
						`When using $ref not other properties are possible (${otherProperties.join(
							", "
						)})`
					);
					process.exitCode = 1;
				}
			}

			if ("type" in item) {
				if (typeof item.type !== "string") {
					console.log(`${path}: should have a single type`);
					process.exitCode = 1;
				}
			}

			if ("instanceof" in item) {
				if (!("tsType" in item)) {
					console.log(`${path}: When using instanceof, tsType is required`);
					process.exitCode = 1;
				}
			}

			if ("absolutePath" in item) {
				if (item.type !== "string") {
					console.log(
						`${path}: When using absolutePath, type must be 'string'`
					);
					process.exitCode = 1;
				}
			}

			if ("properties" in item || "additionalProperties" in item) {
				if (item.type !== "object") {
					console.log(
						`${path}: When using properties or additionalProperties, type must be 'object'`
					);
					process.exitCode = 1;
				}
			}

			arrayProperties.forEach((prop) => {
				if (prop in item) {
					if (item[prop].length === 0) {
						console.log(`${path} should not have empty one/any/allOf`);
						process.exitCode = 1;
					} else if ((item[prop].length === 1) !== (prop === "oneOf")) {
						if (prop === "oneOf") {
							console.log(`${path}: oneOf must have exactly one item`);
						} else {
							console.log(`${path}: ${prop} must have more than one item`);
						}
						process.exitCode = 1;
					}
					let i = 0;
					for (const subitem of item[prop]) {
						if (arrayProperties.some((prop) => prop in subitem)) {
							console.log(`${path} should not double nest one/any/allOf`);
							process.exitCode = 1;
						}
						walker(`${path}.${prop}[${i++}]`, subitem);
					}
				}
			});
			if ("items" in item) {
				validateProperty(`${path}.items`, item.items);
				walker(`${path}.items`, item.items);
			}
			if ("definitions" in item) {
				Object.keys(item.definitions).forEach((name) => {
					const def = item.definitions[name];
					validateProperty(`#${name}`, def);
					walker(`#${name}`, def);
				});
			}
			if ("properties" in item) {
				if (item.additionalProperties === undefined) {
					console.log(
						`${path} should have additionalProperties set to some value when describing properties`
					);
					process.exitCode = 1;
				}
				Object.keys(item.properties).forEach((name) => {
					const property = item.properties[name];
					validateProperty(`${path}.properties.${name}`, property);
					walker(`${path}.properties.${name}`, property);
				});
			}
			if (typeof item.additionalProperties === "object") {
				validateProperty(
					`${path}.additionalProperties`,
					item.additionalProperties
				);
				walker(`${path}.additionalProperties`, item.additionalProperties);
			}
		};

		walker(`[${filename}]`, content);
	}
}
