---
title: "Pattern Matching"
description: "Powerful control flow with case expressions"
order: 4
---

# Pattern Matching

Pattern matching is one of Nanyx's most powerful features. The `case` expression lets you match values against patterns and destructure data.

## Basic Matching

```nanyx
case some_value {
  0 -> "zero"
  1 -> "one"
  _ -> "something else"
}
```

## Matching Custom Types

```nanyx
type Shape {
  Circle(radius: Float)
  Rectangle(width: Float, height: Float)
}

fn area(shape: Shape) -> Float {
  case shape {
    Circle(r) -> 3.14159 *. r *. r
    Rectangle(w, h) -> w *. h
  }
}
```

## Guards

```nanyx
fn describe_number(n: Int) -> String {
  case n {
    n if n < 0 -> "negative"
    0 -> "zero"
    n if n > 100 -> "large"
    _ -> "positive"
  }
}
```

## Destructuring

```nanyx
let #(x, y) = #(10, 20)

case result {
  Ok(value) -> io.println("Got: " <> value)
  Error(msg) -> io.println("Error: " <> msg)
}
```

## Multiple Patterns

```nanyx
case day {
  "Saturday" | "Sunday" -> "Weekend!"
  _ -> "Weekday"
}
```

> **Note:** The Nanyx compiler ensures your patterns are exhaustive — every possible value must be handled. This prevents runtime crashes from unhandled cases.
