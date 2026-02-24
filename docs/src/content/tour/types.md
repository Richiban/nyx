---
title: "Types"
description: "Nanyx's type system"
order: 2
---

# Types

Nanyx has a strong, static type system that catches errors at compile time.

## Primitive Types

```nanyx
let i: Int = 42
let f: Float = 3.14
let s: String = "hello"
let b: Bool = true
let u: Unit = unit    // like void
```

## Lists

```nanyx
let numbers: List(Int) = [1, 2, 3, 4, 5]
let names: List(String) = ["Alice", "Bob"]
let empty: List(Int) = []
```

## Tuples

```nanyx
let pair: #(String, Int) = #("age", 25)
let triple = #(1, 2.0, "three")
```

## Custom Types

Define your own types with the `type` keyword:

```nanyx
type Color {
  Red
  Green
  Blue
  Custom(r: Int, g: Int, b: Int)
}

let sky = Blue
let coral = Custom(255, 127, 80)
```

## Type Aliases

```nanyx
type UserId = Int
type Name = String
type Pair(a, b) = #(a, b)
```

## Generics

```nanyx
type Option(a) {
  Some(a)
  None
}

type Result(value, error) {
  Ok(value)
  Error(error)
}

let maybe_name: Option(String) = Some("Nanyx")
```
