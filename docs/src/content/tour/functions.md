---
title: "Functions"
description: "Defining and calling functions"
order: 3
---

# Functions

Functions are first-class values in Nanyx.

## Defining Functions

```nanyx
fn add(a: Int, b: Int) -> Int {
  a + b
}
```

The last expression is the return value — no `return` keyword needed.

## Public Functions

Use `pub` to export a function from a module:

```nanyx
pub fn greet(name: String) -> String {
  "Hello, " <> name <> "!"
}
```

## Anonymous Functions

```nanyx
let double = fn(x: Int) -> Int { x * 2 }

let numbers = [1, 2, 3]
let doubled = list.map(numbers, fn(x) { x * 2 })
// [2, 4, 6]
```

## Pipelines

The pipe operator `|>` passes the result of one function to the next:

```nanyx
"hello world"
|> string.uppercase
|> string.split(" ")
|> list.first
// Ok("HELLO")
```

This is equivalent to `list.first(string.split(string.uppercase("hello world"), " "))` but much more readable!

## Labeled Arguments

```nanyx
pub fn create_user(name name: String, age age: Int) -> User {
  User(name: name, age: age)
}

// Call with labels in any order
create_user(age: 25, name: "Alice")
```
