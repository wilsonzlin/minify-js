# minify-js

Extremely fast JavaScript minifier, written in Rust.

## Goals

- Fully written in Rust for maximum compatibility with Rust programs and derivatives (FFI, WASM, embedded, etc.).
- Maximises performance on a single CPU core for simple efficient scaling and easy compatible integration.
- Minification of individual inputs/files only; no bundling or transforming.
- Prefer minimal complexity and faster performance over maximum configurability and minimal extra compression.

## Features

- Fast parsing powered by SIMD instructions and lookup tables.
- Minification of identifiers.

## Usage

### CLI

Precompiled binaries are available for Linux, macOS, and Windows.

[Linux x64](https://static.wilsonl.in/minify-js/cli/0.0.2/x86_64/minify-js) |
[macOS x64](https://static.wilsonl.in/minify-js/cli/0.0.2/x86_64/minify-js) |
[Windows x64](https://static.wilsonl.in/minify-js/cli/0.0.2/x86_64/minify-js.exe)

Use the `--help` argument for more details.

```bash
minify-js --output /path/to/output.min.js /path/to/src.js
```

### Rust

Add the dependency:

```toml
[dependencies]
minify-js = "0.0.2"
```

Call the method:

```rust
use minify_js::minify;

let mut code: &[u8] = b"let x = 1;";
let mut out = Vec::new();
minify(code.to_vec(), &mut out).unwrap();
assert_eq!(out.as_slice(), b"let x=1");
```

## In progress

- Combine and reorder declarations.
- Omit more semicolons, spaces, parentheses, and braces.
- Minify import and export syntax.
- More extensive testing, especially over rare syntax.
- Evaluation and folding of constant expressions.
- Parse and erase TypeScript syntax.
- FFI libraries for other languages.
- Aliasing of reused well-knowns.
- Removal of unreachable and redundant code.
- Aliasing frequently accessed properties and called methods.
- Better support for non-ASCII identifiers.
- Replacing if statements with conditional and logical expressions.
- Aliasing repeated identical literal values.
- Micro-optimisations:
  - Unwrap string literal computed members, then identifier or number string members.
  - Replace `x === null || x === undefined` with `x == null`, where `x` is side-effect free.
  - Using shorthand properties and Object.assign.
  - (Dangerous) Replace functions without use of `this` with arrow functions.
  - Replace `void x` with `undefined`, where `x` is side-effect free.
  - Replace `return undefined` with `return`.
  - Replace `const` with `let`.
  - Hoist `let` and `const`.
  - Unwrapping blocks.
  - Unwrapping paretheses, altering expressions as necessary.
