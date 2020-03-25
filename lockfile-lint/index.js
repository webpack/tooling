const { root } = require("../lib/argv");

const fs = require("fs");
const path = require("path");
const lockfile = require("@yarnpkg/lockfile");

const file = fs.readFileSync(path.resolve(root, "yarn.lock"), "utf-8");
const result = lockfile.parse(file);

if (result.type !== "success") {
	console.log("Unable to parse lockfile");
	process.exitCode = 1;
} else {
	const content = result.object;
	for (const dep of Object.keys(content)) {
		if (/^tooling@/.test(dep)) continue;
		const info = content[dep];
		if (!/^https:\/\/registry\.yarnpkg\.com\//.test(info.resolved)) {
			console.log(`${dep} should resolve to an npm package`);
			process.exitCode = 1;
		}
		if (!/^(sha1|sha512)-/.test(info.integrity)) {
			console.log(`${dep} should have an integrity hash`);
			process.exitCode = 1;
		}
	}
}
