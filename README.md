# minify-js

Extremely fast JavaScript minifier, written in Rust.

Currently usable, but under development, with the goal to be similar in effectiveness to common minifiers including [esbuild](https://github.com/wilsonzlin/esbuild-rs) and [terser](https://github.com/terser/terser). Improvements and suggestions welcome!

## Goals

- Fully written in Rust for maximum compatibility with Rust programs and derivatives (FFI, WASM, embedded, etc.).
- Maximises performance on a single CPU core for simple efficient scaling and easy compatible integration.
- Minification of individual inputs/files only; no bundling or transforming.
- Prefer minimal complexity and faster performance over maximum configurability and minimal extra compression.

## Features

- Fast parsing powered by SIMD instructions and lookup tables.

## Usage

Add the dependency:

```toml
[dependencies]
minify-js = "0.0.2"
```

Call the method:

```rust
use std::io::BufWriter;
use minify_js::minify;

let mut code: &[u8] = b"let x = 1;";
let mut out = BufWriter::new(Vec::new());
minify(code.to_vec(), &mut out).unwrap();
assert_eq!(out.get_ref().as_slice(), b"let x=1;");
```

## In progress

- Minify identifiers.
- Combine and reorder declarations.
- Omit more semicolons, spaces, parentheses, and braces.
- More extensive testing, especially over rare syntax.
- Evaluation and folding of constant expressions.
- Parse and erase TypeScript syntax.
- FFI libraries for other languages.
- Aliasing of reused well-knowns.
- Removal of unreachable code.
- Unwrap string literal computed members.
- Aliasing frequently accessed members and methods.
- Better support for non-ASCII syntax.
