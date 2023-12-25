# minify-js

Extremely fast JavaScript minifier, written in Rust.

## Goals

- Fully written in Rust for maximum compatibility with Rust programs and derivatives (FFI, WASM, embedded, etc.).
- Maximises performance on a single CPU core for simple efficient scaling and easy compatible integration.
- Minification of individual inputs/files only; no bundling or transforming.
- Prefer minimal complexity and faster performance over maximum configurability and minimal extra compression.

## Performance

Comparison with esbuild, run on [common libraries](./bench).

<img width="400" alt="Chart showing speed of JS minifiers" src="https://static.wilsonl.in/minify-js/bench/0.6.0/total-times.svg"><img width="400" alt="Chart showing compression of JS minifiers" src="https://static.wilsonl.in/minify-js/bench/0.6.0/average-sizes.svg">

## Features

- Fast parsing powered by SIMD instructions and lookup tables.
- Data is backed by a fast reusable bump allocation arena.
- Supports JSX.
- Analyses scopes and variable visibilities.
- Minifies identifiers.
- Omits semicolons, spaces, parentheses, and braces where possible.
- Transforms functions to arrow functions when `new`, `this`, `arguments`, and `prototype` aren't used.
- Transforms `if` statements to expressions.

## Usage

### CLI

Precompiled binaries are available for Linux, macOS, and Windows.

[Linux x64](https://static.wilsonl.in/minify-js/cli/0.6.0/linux-x86_64/minify-js) |
[macOS x64](https://static.wilsonl.in/minify-js/cli/0.6.0/macos-x86_64/minify-js) |
[Windows x64](https://static.wilsonl.in/minify-js/cli/0.6.0/windows-x86_64/minify-js.exe)

Use the `--help` argument for more details.

```bash
minify-js --output /path/to/output.min.js /path/to/src.js
```

### Rust

Add the dependency:

```toml
[dependencies]
minify-js = "0.6.0"
```

Call the method:

```rust
use minify_js::{Session, TopLevelMode, minify};

let mut code: &[u8] = b"const main = () => { let my_first_variable = 1; };";
let session = Session::new();
let mut out = Vec::new();
minify(&session, TopLevelMode::Global, code, &mut out).unwrap();
assert_eq!(out.as_slice(), b"const main=()=>{let a=1}");
```

### Node.js

Install the dependency:

```bash
npm i @minify-js/node
```

Call the method:

```typescript
import {minify} from "@minify-js/node";

const src = Buffer.from("let x = 1;", "utf-8");
const min = minify(src);
```

## In progress

- Combine and reorder declarations.
- Evaluation and folding of constant expressions.
- Parse and erase TypeScript syntax.
- Removal of unreachable, unused, and redundant code.
- Inlining single-use declarations.
- Replacing if statements with conditional and logical expressions.
- Returning an explicit error on illegal code e.g. multiple declarations/exports with identical names.
- Much more inline, high level, and usage documentation.
- Support import and export string names e.g. `import { "a-b" as "c-d" } from "x"`.
- Simplify pattern parsing and minification.
- Micro-optimisations:
  - Unwrap string literal computed members, then identifier or number string members.
  - Replace `x === null || x === undefined` with `x == null`, where `x` is side-effect free.
  - Replace `typeof x === "undefined"` with `x === undefined`.
  - Using shorthand properties.
  - Replace `void x` with `x, undefined`.
  - Replace `return undefined` with `return`.
  - Replace `const` with `let`.
  - Hoist `let` and `const`.
  - Unwrapping blocks.
  - Unwrapping paretheses, altering expressions as necessary.
  - `if (...) return a; else if (...) return b; else return c` => `return (...) ? a : (...) ? b : c`.
