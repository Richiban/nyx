---
title: "Variables"
description: "Def bindings and immutability in Nanyx"
order: 1
---

# Variables

In Nanyx, variables are immutable by default. You declare them using the `def` keyword.

## Def Bindings

```nanyx
def name = "Nanyx"
def version = 4
def pi = 3.14159
```

Once a value is bound, it cannot be reassigned:

```nanyx
def x = 10
x = 20  // ✗ Compile error: cannot reassign immutable variable
```

## Mutable Variables

When you need mutation, Nanyx supports _bounded mutability_ using the `memory` context:

```nanyx
memory {
  def counter = mut(0)
  counter := counter + 1  // ✓ This works
}
```

> **Tip:** Prefer immutable bindings whenever possible. They make your code easier to reason about.

## Type Annotations

Nanyx can infer types, but you can add annotations for clarity:

```nanyx
def name: String = "Nanyx"
def count: Int = 42
def ratio: Float = 0.75
def active: Bool = true
```

## Blocks

A block expression returns the value of its last expression:

```nanyx
def result =
  def a = 10
  def b = 20
  a + b

// result is 30
```
