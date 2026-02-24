---
title: "nanyx/string"
description: "String manipulation functions"
order: 3
---

# nanyx/string

The `string` module provides functions for working with UTF-8 strings.

## Functions

### `length(s: String) -> Int`

Returns the number of grapheme clusters in the string.

```nanyx
string.length("hello")
// 5
```

### `uppercase(s: String) -> String`

```nanyx
string.uppercase("hello")
// "HELLO"
```

### `lowercase(s: String) -> String`

```nanyx
string.lowercase("HELLO")
// "hello"
```

### `split(s: String, on: String) -> List(String)`

```nanyx
string.split("a,b,c", on: ",")
// ["a", "b", "c"]
```

### `contains(s: String, sub: String) -> Bool`

```nanyx
string.contains("hello world", "world")
// True
```

### `replace(s: String, pattern: String, with: String) -> String`

```nanyx
string.replace("hello world", "world", "Nanyx")
// "hello Nanyx"
```

### `trim(s: String) -> String`

Removes leading and trailing whitespace.

```nanyx
string.trim("  hello  ")
// "hello"
```

### `concat(strings: List(String)) -> String`

```nanyx
string.concat(["Hello", ", ", "World"])
// "Hello, World"
```
