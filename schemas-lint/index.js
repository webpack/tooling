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

		const validateProperty = (path, property) => {
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
					`The properties ${otherProperties.join(", ")} are not allowed to use`
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
					let i = 0;
					for (const x of item[prop]) walker(`${path}.${prop}[${i++}]`, x);
				}
			});
			if ("items" in item) {
				if (Object.keys(item.items).join() !== "$ref") {
					validateProperty(`${path}.items`, item.items);
				}
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
				}
				Object.keys(item.properties).forEach((name) => {
					const property = item.properties[name];
					if (Object.keys(property).join() !== "$ref") {
						validateProperty(`${path}.properties.${name}`, property);
					}
					walker(`${path}.properties.${name}`, property);
				});
			}
			if (typeof item.additionalProperties === "object") {
				if (Object.keys(item.additionalProperties).join() !== "$ref") {
					validateProperty(
						`${path}.additionalProperties`,
						item.additionalProperties
					);
				}
				walker(`${path}.additionalProperties`, item.additionalProperties);
			}
		};

		walker(`[${filename}]`, content);
	}
}
