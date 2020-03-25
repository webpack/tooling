# tooling

A collection of reusable tooling for webpack repos

## Setup

Add this repo as dev dependency:

```json
"devDependencies": {
    "tooling": "webpack/tooling"
}
```

The lockfile will take care pinning the version.
Run `yarn upgrade tooling` to upgrade the to latest version.

Add two scripts to the package.json:

```json
"scripts": {
    "special-lint": "...",
    "special-lint-fix": "..."
}
```

Add all tooling that should be used to these scripts (concatenated with `&&`). In the `special-lint-fix` version pass `--write` to all tooling.

Example:

```json
"scripts": {
    "special-lint": "node node_modules/tooling/compile-to-definitions",
    "special-lint-fix": "node node_modules/tooling/compile-to-definitions --write"
}
```

## Common arguments

By default all tooling checks if affected files are up-to-date, so it can be added as linting step in CI.
Use `--write` to switch to updating mode.
In this mode affected files are updated to the fixed version when possible.

By default all tooling will operate in the root directory of the calling project.
Use `--root some/dir` to operate in a different directory.

Some tooling displays more information when `--verbose` is used.

Most tooling uses prettier to format generated files, so a prettier config is needed and prettier need to be installed.

## lockfile-lint

```sh
node node_modules/tooling/lockfile-lint
```

Verifies the correctness of a yarn lockfile.
Makes sure that all dependencies are provided from npm (e. g. not github dependencies).
This is important as some users might not be able to access github.

`yarn.lock` must be present in root directory.

## schemas-lint

```sh
node node_modules/tooling/schemas-lint
```

Verifies the correctness of all JSON schemas.

- Definitions and properties need to a `description` in the correct format.
- Only allowed properties are used.
- `$ref` must be the only property when provided.
- `tsType` is needed when `instanceof` is used.
- `type: "string"` is needed when `absolutePath` is used.
- `additionalProperties` is needed when `properties` are used.

```text
--schemas ./schemas/**/*.json
```

Glob to the schemas that should be processed.

## compile-to-definitions

```sh
node node_modules/tooling/compile-to-definitions
```

Generates typescript declaration files from JSON schemas.

```text
--schemas ./schemas/**/*.json
```

Glob to the schemas that should be processed.

```text
---declarations declarations
```

Output folder of the generated declaration files.

## inherit-types

```sh
node node_modules/tooling/inherit-types
```

Synchronize jsdoc method annotations in classes with jsdoc of the same method in base class.
This copies jsdoc one to one from base class, but omits `@abstract`.

`tsconfig.json` must be present in root directory and typescript must be installed.

## format-file-header

```sh
node node_modules/tooling/format-file-header
```

Ensures that the starting of all source files follows the following convention:

```js
/*
	MIT License http://www.opensource.org/licenses/mit-license.php
	Author ...
*/

"use strict";

const Import = require("./Import");
const SortedAlphabetically = require("./SortedAlphabetically");

/** @typedef {import("../TypeImport")} TypeImport */
/** @typedef {import("../SortedAlphabetically")} SortedAlphabetically */
```

```text
--source ./lib/**/*.js
```

Glob to the source that should be processed.

## format-schemas

```sh
node node_modules/tooling/format-schemas
```

Sort JSON schema according to convention.

```text
--schemas ./schemas/**/*.json
```

Glob to the schemas that should be processed.

## generate-types

```sh
node node_modules/tooling/generate-types
```

Generate typescript types declarations file (`types.d.ts`) from the visible types in the exposed `main` (package.json) entry file.
This declaration file should be used as `types` in `package.json`.

`tsconfig.types.json` must be present in root directory and typescript must be installed.

```text
--types types.d.ts
```

Path of the generated declarations file.

## type-coverage

```sh
node node_modules/tooling/generate-types
```

Generates type coverage raw coverage data in the `coverage` directory.
`instanbul report` can be used to generate a readable report.

`tsconfig.json` must be present in root directory and typescript must be installed.
