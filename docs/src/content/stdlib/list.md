---
title: "nanyx/list"
description: "Operations on lists"
order: 2
---

# nanyx/list

The `list` module provides functions for working with lists.

## Functions

### `map(list: List(a), f: fn(a) -> b) -> List(b)`

Applies a function to every element in a list.

```nanyx
list.map([1, 2, 3], fn(x) { x * 2 })
// [2, 4, 6]
```

### `filter(list: List(a), f: fn(a) -> Bool) -> List(a)`

Keeps only elements that satisfy the predicate.

```nanyx
list.filter([1, 2, 3, 4, 5], fn(x) { x > 3 })
// [4, 5]
```

### `fold(list: List(a), init: b, f: fn(b, a) -> b) -> b`

Reduces a list to a single value.

```nanyx
list.fold([1, 2, 3], 0, fn(acc, x) { acc + x })
// 6
```

### `find(list: List(a), f: fn(a) -> Bool) -> Option(a)`

Returns the first element matching the predicate.

```nanyx
list.find([1, 2, 3], fn(x) { x > 1 })
// Some(2)
```

### `flatten(lists: List(List(a))) -> List(a)`

Flattens a list of lists into a single list.

```nanyx
list.flatten([[1, 2], [3, 4]])
// [1, 2, 3, 4]
```

### `length(list: List(a)) -> Int`

Returns the number of elements.

```nanyx
list.length([1, 2, 3])
// 3
```
