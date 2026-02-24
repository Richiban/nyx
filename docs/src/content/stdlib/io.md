---
title: "nanyx/io"
description: "Input and output operations"
order: 1
---

# nanyx/io

The `io` module provides functions for reading from and writing to standard I/O.

## Functions

### `println(message: String) -> Unit`

Prints a string followed by a newline to stdout.

```nanyx
io.println("Hello, World!")
```

### `print(message: String) -> Unit`

Prints a string to stdout without a trailing newline.

```nanyx
io.print("Enter your name: ")
```

### `debug(value: a) -> a`

Prints a debug representation of any value and returns it. Useful for inspecting values in pipelines.

```nanyx
[1, 2, 3]
|> list.map(fn(x) { x * 2 })
|> io.debug
// Prints: [2, 4, 6]
```

### `read_line() -> Result(String, IoError)`

Reads a line of input from stdin.

```nanyx
case io.read_line() {
  Ok(line) -> io.println("You said: " <> line)
  Error(_) -> io.println("Failed to read input")
}
```
