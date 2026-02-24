---
title: "Variables"
description: "Let bindings and immutability in Nanyx"
order: 1
---

# Variables

In Nanyx, variables are immutable by default. You declare them using the `let` keyword.

## Let Bindings

```nanyx
let name = "Nanyx"
let version = 4
let pi = 3.14159
```

Once a value is bound, it cannot be reassigned:

```nanyx
let x = 10
x = 20  // ✗ Compile error: cannot reassign immutable variable
```

## Mutable Variables

When you need mutation, use `let mut`:

```nanyx
let mut counter = 0
counter = counter + 1  // ✓ This works
```

> **Tip:** Prefer immutable bindings whenever possible. They make your code easier to reason about.

## Type Annotations

Nanyx can infer types, but you can add annotations for clarity:

```nanyx
let name: String = "Nanyx"
let count: Int = 42
let ratio: Float = 0.75
let active: Bool = true
```

## Blocks

A block expression returns the value of its last expression:

```nanyx
let result = {
  let a = 10
  let b = 20
  a + b
}
// result is 30
```
