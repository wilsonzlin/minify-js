# Lexical lifetimes

- Assignments are like new variables that already optimally reuse a variable slot.
  - It's why we're trying to analyse lifetimes: so we can reduce the amount of var decls. In the same way, the reverse is true.
  - This is useful for loops, as their lifetime is related to their assignment (whereas others only care about uses). If the first assignment (not declaration, as it could be declared outside but not used outside) is inside the loop and before any use, then it is local to the loop. Otherwise, we may incorrectly assume lifetime ends within loop and assign its var to something else, cobbling the value between iterations.
    - If the first appearance isn't an unconditional assignment, it means it depends on a value from some previous iteration or before the loop or a nested `if`/`for`/etc. If it is, then we can safely reuse its assigned variable and clobber any existing value because it won't care what it is anyway.
      - Therefore, if the first appearance isn't assignment, make its lifetime end after the loop, no matter where it appears last in the loop. Obviously if it appears after the loop then none of this applies.
  - This also detects pointless unused assignments.

- Assignments except `=` (e.g. `+=`, `??=`) are usages first and then assignments second (and then usages again). `=` are assignments first and then usages second (but the RHS may reference the variable as a usage).
- Handle destructuring carefully. Destructuring may be pointless if we end up minifying names as we're unlikely to be able to use shorthands. Therefore, it may be optimal to transform destructurings into multiple individual assignments, which would also give opportunities to split them into hoistable var decls and assignment expressions.
- Usages from nested closures count as infinite lifetime. This includes classes, as even class property initialisations re-evaluate the variable on each constructor call.

```js
var {foo: [bar, {baz}]} = obj;
// Name minification only:
var {foo: [a, {baz: b}]} = obj;
// Unpacked:
var a = obj.foo,
    b = a[0],
    c = a[1],
    d = c.baz;
```

When branching, if a lifetime starts and/or ends in a branch, then optimal minimum set of groups is different per branch. However, this is quite tricky to code for. Consider that the optimal algorithm is global, but we're only allowed to modify the parts of the solution within the branch; everything else must remain locked in. Therefore, for simplicity, we'll just use the maximum lifetime value of all branches for a symbol, which is not optimal but good enough.

Lifetime values have no meaning by themselves; they are only useful when compared to other values.

- For each symbol assignment (IdentifierPattern), incr top of stack and start new lifetime on symbol if existing lifetime end is before.
- For each symbol usage (IdentifierExpr) not in a loop, incr top of stack.
- For each symbol usage (IdentifierExpr) in a loop, check current lifetime's end:
  - If same level, it means there was an unconditional assignment in the same loop somewhere before. Incr top of stack.
  - Otherwise, don't modify stack, and instead use the current loop's end lifetime as its lifetime.
- For each conditional, incr top of stack. For each branch, push stack 0.
- For each loop, incr top of stack. Before entering, push stack 0. Mark as in loop. After exiting, incr top of stack again. Therefore, a loop has two lifetime values: start and end.

```js
function foo() {
  var a, b = f(a), c = 2;

  a = 1;
  f(a);

  if (true) {
    let d = 1;
    a = 2;
    var e = 2;
  } else {
    a = 4;
    e = 3;
  }

  if (true) {
    e = 4;
    return;
  }
  f(e);

  for (;;) {
    var x = 2;
    a = a + 1;
    b++;
    c = 2;
  }

  f(b);
}
```

```js
// Example of how assignments start new lifetimes.
var a, b;

a = a + (b = 2 * 4) // Lifetimes don't overlap, can technically become `a = a + (a = 2 * 4)`.

a = (b = 2 * 4) + a // Lifetimes overlap, can't be reduced to one (unless we do nifty optimisations around reordering).
```

```js
// Example of how assignments start lifetimes. If not, this `x` would have unnecessarily long lifetime.
x = 1;
foo();
bar();
baz();
x = 2;
```

```js
// Mental puzzle.
if (foo) {
  for (;;) {
    if (bar) {
      for (;;) {
        x = 1;
      }
      x = 1;
    }
    x = 1;
  }
  x = 1;
}
```

```js
// Example of how usages in nested closures are unknowable and so must be infinite.
var x = 1;
(() => {
  if (Math.random() < 0.5) {
    x = 2;
  } else {
    setInterval(() => {
      x++;
    }, 1000);
  }
})();
var y = 2;
```
