---
title: "string"
description: "String manipulation functions"
order: 3
---

# string

The `string` module provides functions for working with UTF-8 strings.

## Functions

### `length: string -> int`

Returns the number of grapheme clusters in the string.

```nyx
string.length("hello")
-- 5
```

### `uppercase: string -> string`

```nyx
string.uppercase("hello")
-- "HELLO"
```

### `lowercase: string -> string`

```nyx
string.lowercase("HELLO")
-- "hello"
```

### `split: (string, on: string) -> list(string)`

```nyx
string.split("a,b,c", on = ",")
-- ["a", "b", "c"]
```

### `contains: (string, string) -> bool`

```nyx
string.contains("hello world", "world")
-- true
```

### `replace: (string, pattern: string, with: string) -> string`

```nyx
string.replace("hello world", pattern = "world", with = "Nanyx")
-- "hello Nanyx"
```

### `trim: string -> string`

Removes leading and trailing whitespace.

```nyx
string.trim("  hello  ")
-- "hello"
```

### `concat: list(string) -> string`

```nyx
string.concat(["Hello", ", ", "World"])
-- "Hello, World"
```
