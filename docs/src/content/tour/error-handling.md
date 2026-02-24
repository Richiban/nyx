---
title: "Error Handling"
description: "Working with Result and Option types"
order: 6
---

# Error Handling

Nanyx uses the `Result` and `Option` types for error handling — no exceptions!

## The Result Type

```nanyx
type Result(value, error) {
  Ok(value)
  Error(error)
}
```

Functions that can fail return a `Result`:

```nanyx
pub fn parse_int(input: String) -> Result(Int, String) {
  // ...
}

case parse_int("42") {
  Ok(n) -> io.println("Got: " <> int.to_string(n))
  Error(e) -> io.println("Failed: " <> e)
}
```

## The `try` Keyword

Use `try` to short-circuit on errors:

```nanyx
pub fn process(input: String) -> Result(Int, String) {
  try value = parse_int(input)
  try doubled = safe_multiply(value, 2)
  Ok(doubled)
}
```

If any step returns `Error`, the function immediately returns that error.

## The Option Type

```nanyx
type Option(a) {
  Some(a)
  None
}

fn find_user(id: Int) -> Option(User) {
  case id {
    1 -> Some(User("Alice"))
    _ -> None
  }
}
```

## Chaining with Pipelines

```nanyx
input
|> parse_int
|> result.map(fn(n) { n * 2 })
|> result.unwrap(or: 0)
```

> **Philosophy:** By making errors explicit in the type system, Nanyx ensures you always handle failure cases. No more unexpected crashes!
