const { root } = require("./argv");

const path = require("path");
const fs = require("fs");
const ts = require("typescript");

const configPath = path.resolve(root, "tsconfig.json");
const configContent = fs.readFileSync(configPath, "utf-8");
const configJsonFile = ts.parseJsonText(configPath, configContent);
const parsedConfig = ts.parseJsonSourceFileConfigFileContent(
	configJsonFile,
	ts.sys,
	root,
	{ noEmit: true }
);
const { fileNames, options } = parsedConfig;

module.exports = ts.createProgram(fileNames, options);
