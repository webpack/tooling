module.exports = require("yargs")
	.boolean("write")
	.describe(
		"write",
		"Write updated files to disk, otherwise it only checks if they are correct."
	)

	.boolean("verbose")
	.describe("verbose", "Print more info to output.")

	.string("root")
	.describe(
		"root",
		"Root repository directory (optional if calling from package.json scripts)."
	)
	.default(
		"root",
		process.env.INIT_CWD || process.cwd(),
		"The root directory from calling package.json or the current directory"
	)

	.string("schemas")
	.describe("schemas", "Glob to find schemas in root directory.")
	.default("schemas", "./schemas/**/*.json")

	.string("declarations")
	.describe(
		"declarations",
		"Output folder for declarations generated from schemas."
	)
	.default("declarations", "declarations")

	.string("source")
	.describe("source", "Glob to find source code in root directory.")
	.default("source", "./lib/**/*.js")

	.string("types")
	.describe("types", "Output file for types declarations.")
	.default("types", "types.d.ts")

	.parse();
