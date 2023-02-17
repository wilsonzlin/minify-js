# Textual compression

We do not perform any textual compression:

- They make minification much more expensive and complex.
- They have a runtime cost due to indirection and escaping engine optimisations.
- They almost always worsen compression (e.g. gzip, Brotli) and those algorithms already perform the same functionality.
- They pollute the scope and increase the amount of variables, which can reduce name minification effectiveness.
- They are tricky to get correct, since they often (can) alter symbols and execution stacks and orders.

Some forms include:

- Aliasing of reused well-knowns.
- Aliasing repeated identical literal values.
- Aliasing frequently accessed properties and called methods.
- Using Object.assign.
- Replacing `typeof` and `instanceof` with functions.
- Replacing common statement patterns with helper functions.

Aliasing repeated constants (including built-in variables and properties) was dropped in commit [ff48b8b](https://github.com/wilsonzlin/minify-js/commit/ff48b8b).
