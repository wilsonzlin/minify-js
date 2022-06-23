# minify-js

Extremely fast JavaScript minifier, written in Rust.

## Goals

- Fully written in Rust for maximum compatibility with Rust programs and derivatives (FFI, WASM, embedded, etc.).
- Maximises performance on a single CPU core for simple efficient scaling and easy compatible integration.
- Minification of individual inputs/files only; no bundling or transforming.
- Prefer minimal complexity and faster performance over maximum configurability and minimal extra compression.

## Performance

Comparison with esbuild, run on [common libraries](./bench).

<img width="400" alt="Chart showing speed of JS minifiers" src="https://static.wilsonl.in/minify-js/bench/0.1.5/total-times.svg"><img width="400" alt="Chart showing compression of JS minifiers" src="https://static.wilsonl.in/minify-js/bench/0.1.5/average-sizes.svg">

## Features

- Fast parsing powered by SIMD instructions and lookup tables.
- Minification of identifiers.
- Omits semicolons, spaces, parentheses, and braces where possible.

## Usage

### CLI

Precompiled binaries are available for Linux, macOS, and Windows.

[Linux x64](https://static.wilsonl.in/minify-js/cli/0.1.5/linux-x86_64/minify-js) |
[macOS x64](https://static.wilsonl.in/minify-js/cli/0.1.5/macos-x86_64/minify-js) |
[Windows x64](https://static.wilsonl.in/minify-js/cli/0.1.5/windows-x86_64/minify-js.exe)

Use the `--help` argument for more details.

```bash
minify-js --output /path/to/output.min.js /path/to/src.js
```

### Rust

Add the dependency:

```toml
[dependencies]
minify-js = "0.1.5"
```

Call the method:

```rust
use minify_js::minify;

let mut code: &[u8] = b"let x = 1;";
let mut out = Vec::new();
minify(code.to_vec(), &mut out).unwrap();
assert_eq!(out.as_slice(), b"let x=1");
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
- Minify import and export syntax.
- Evaluation and folding of constant expressions.
- Parse and erase TypeScript syntax.
- Removal of unreachable, unused, and redundant code.
- Inlining single-use declarations.
- Replacing if statements with conditional and logical expressions.
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

### Textual compression

- Aliasing of reused well-knowns.
- Aliasing repeated identical literal values.
- Aliasing frequently accessed properties and called methods.
- Using Object.assign.
- Replace `typeof` and `instanceof` with functions.
