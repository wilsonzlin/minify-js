# Name minification

## Algorithm

- For each symbol usage:
  - For each scope starting from the current up to and including the top-most:
    - If symbol is declared in the scope, break.
    - Mark symbol as inherited on the scope.

### Clear example

```js
// Inherited: bar, baz, top0, top1, top2.
let foo;
foo; bar; baz; top0;
{
  // Inherited: foo, baz, top1, top2.
  let bar;
  foo; bar; baz; top1;
  {
    // Inherited: foo, bar, top2.
    let baz;
    foo; bar; baz; top2;
  }
}
```

### More obfusicated example

```js
// Inherited: e, d, c, b, a.
let f;
f; e; d; c;
{
  // Inherited: f, d, b, a.
  let e;
  f; e; d; b;
  {
    // Inherited: f, e, a.
    let d;
    f; e; d; a;
  }
}
```
