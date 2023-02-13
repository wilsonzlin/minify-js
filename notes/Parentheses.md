# Parentheses

## When are parentheses needed outside of standard operator precedence rules?

### When a comma would not be interpreted as a comma expression

- Call arguments: `fn(a, (1, 2))`.
- Variable intialisers: `let x = 1, y = (2, 3), z = 4`.
- Return expression of arrow function (because it has lower precedence): `() => (1, 2)`.
- Array and object literals: `[1, (2, 3)]`, `{a: 1, b: (2, 3)}`.

### When an expression would be misinterpreted as a statement

- Object destructuring without declaration or literal (misinterpreted as block): `({prop1: var1, prop2: [var2, var3]} = {})`, `() => ({a: 1})`.
- Function expression (misinterpreted as declaration, which has ramifications on IIFE and visibility): `(function decl() {})()`.
