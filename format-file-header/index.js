const argv = require("../lib/argv");

const { write: doWrite, root, source: sourceGlob } = argv;

const path = require("path");
const fs = require("fs");
const glob = require("glob");

const allFiles = glob.sync(sourceGlob, { cwd: root, absolute: true });

let canUpdateWithWrite = false;

const sortImport = (a, b) => {
	if (!a.key.startsWith(".") && b.key.startsWith(".")) return -1;
	if (a.key.startsWith(".") && !b.key.startsWith(".")) return 1;
	if (a.key < b.key) return -1;
	if (a.key > b.key) return 1;
	return 0;
};

const execToArray = (content, regexp) => {
	const items = [];
	let match = regexp.exec(content);
	while (match) {
		items.push({
			content: match[0],
			key: match[1] + match[2],
		});
		match = regexp.exec(content);
	}
	return items;
};

/**
 * @typedef {Object} Schema
 * @property {string} title
 * @property {RegExp} regexp
 * @property {string=} updateMessage
 * @property {boolean=} optional
 * @property {boolean=} repeat
 * @property {(...match: string[]) => string=} update
 */

/** @type {Schema[]} */
const schema = [
	{
		title: "ts-ignore",
		regexp: /(\/\/ @ts-nocheck\n)?/g,
	},
	{
		title: "license comment",
		regexp:
			/\/\*\n\s*MIT License http:\/\/www\.opensource\.org\/licenses\/mit-license\.php\n\s*(?:(Authors? .+)\n)?\s*\*\/\n/g,
		updateMessage: "update the license comment",
		update(content, author) {
			return (
				[
					"/*",
					"\tMIT License http://www.opensource.org/licenses/mit-license.php",
					author && `\t${author}`,
					"*/",
				]
					.filter(Boolean)
					.join("\n") + "\n"
			);
		},
	},
	{
		title: "new line after license comment",
		regexp: /\n?/g,
		updateMessage: "insert a new line after the license comment",
		update() {
			return "\n";
		},
	},
	{
		title: "strict mode",
		regexp: /"use strict";\n/g,
	},
	{
		title: "new line after strict mode",
		regexp: /\n?/g,
		updateMessage: 'insert a new line after "use strict"',
		update() {
			return "\n";
		},
	},
	{
		title: "imports",
		regexp:
			/(const (\{\s+\w+(?::\s+\w+)?(?:,)?(,\s+\w+(?::\s+\w+)?(?:,)?)*\s+\}|\w+) = (\/\*\* @type \{TODO\} \*\/\s\()?require\("[^"]+"\)\)?(\.\w+)*;\n)+\n/g,
		updateMessage: "sort imports alphabetically",
		update(content) {
			const items = execToArray(
				content,
				/const (?:\{\s+\w+(?::\s+\w+)?(?:,)?(?:,\s+\w+(?::\s+\w+)?(?:,)?)*\s+\}|\w+) = (?:\/\*\* @type \{TODO\} \*\/\s\()?require\("([^"]+)"\)\)?((?:\.\w+)*);\n/g,
			);
			items.sort(sortImport);
			return items.map((item) => item.content).join("") + "\n";
		},
		optional: true,
		repeat: true,
	},
	{
		title: "type imports",
		regexp:
			/(\/\*\* (?:@template \w+ )*@typedef \{(?:typeof )?import\("[^"]+"\)(\.\w+)*(?:<(?:(?:\w\.)*\w+, )*(?:\w\.)*\w+>)?\} \w+(?:<(?:(?:\w\.)*\w+, )*(?:\w\.)*\w+>)? \*\/\n)+\n/g,
		updateMessage: "sort type imports alphabetically",
		update(content) {
			const items = execToArray(
				content,
				/\/\*\* (?:@template \w+ )*@typedef \{(?:typeof )?import\("([^"]+)"\)((?:\.\w+)*(?:<(?:(?:\w\.)*\w+, )*(?:\w\.)*\w+>)?)\} \w+(?:<(?:(?:\w\.)*\w+, )*(?:\w\.)*\w+>)? \*\/\n/g,
			);
			items.sort(sortImport);
			return items.map((item) => item.content).join("") + "\n";
		},
		optional: true,
		repeat: true,
	},
];

const allSerializables = new Set();

for (const filePath of allFiles) {
	let content = fs.readFileSync(filePath, "utf-8");
	const nl = /(\r?\n)/.exec(content);
	content = content.replace(/\r?\n/g, "\n");
	let newContent = content;

	let state = 0;
	let pos = 0;
	// eslint-disable-next-line no-constant-condition
	while (true) {
		const current = schema[state];
		if (!current) break;
		current.regexp.lastIndex = pos;
		const match = current.regexp.exec(newContent);
		if (!match) {
			if (current.optional) {
				state++;
				continue;
			}
			console.log(`${filePath}: Missing ${current.title} at ${pos}`);
			process.exitCode = 1;
			break;
		}
		if (match.index !== pos) {
			console.log(
				`${filePath}: Unexpected code at ${pos}-${match.index}, expected ${current.title}`,
			);
			process.exitCode = 1;
			pos = match.index;
		}
		if (!current.repeat) {
			state++;
		}
		if (current.update) {
			const update = current.update(...match);
			if (update !== match[0]) {
				newContent =
					newContent.slice(0, pos) +
					update +
					newContent.slice(pos + match[0].length);
				pos += update.length;
				if (!doWrite) {
					const updateMessage =
						current.updateMessage || `${current.title} need to be updated`;
					console.log(`${filePath}: ${updateMessage}`);
				}
				continue;
			}
		}
		pos += match[0].length;
	}

	if (newContent !== content) {
		if (doWrite) {
			if (nl) {
				newContent = newContent.replace(/\n/g, nl[0]);
			}
			fs.writeFileSync(filePath, newContent, "utf-8");
			console.log(filePath);
		} else {
			console.log(filePath + " need to be updated.");
			canUpdateWithWrite = true;
			process.exitCode = 1;
		}
	}

	const matches = content.match(
		/makeSerializable\(\s*[^,]+,\s*"webpack\/lib\/[^"]+"\s*(?:,[^)]+)?\)/g,
	);
	if (matches) {
		for (const match of matches) {
			const str = /makeSerializable\(\s*[^,]+,\s*"webpack\/lib\/([^"]+)"/.exec(
				match,
			)[1];
			allSerializables.add(str);
		}
	}
}

// Check if internalSerializables.js includes all serializables in webpack
for (const internalSerializables of allFiles.filter((file) =>
	file.includes("internalSerializables"),
)) {
	const content = fs.readFileSync(internalSerializables);
	for (const serializable of allSerializables) {
		if (!content.includes(`"../${serializable}"`)) {
			console.log(
				`${internalSerializables}: must include static require to ../${serializable}`,
			);
			process.exitCode = 1;
		}
	}
}

if (canUpdateWithWrite) {
	console.log("Run 'yarn fix' to try to fix the problem automatically");
}
