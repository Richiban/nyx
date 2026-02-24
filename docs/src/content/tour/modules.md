---
title: "Modules"
description: "Organizing code with modules"
order: 5
---

# Modules

Nanyx uses a module system to organize code. Each `.nx` file is a module.

## Module Structure

A module's name matches its file path:

```
src/
├── app.nx              → app
├── models/
│   ├── user.nx         → models/user
│   └── post.nx         → models/post
└── utils/
    └── helpers.nx      → utils/helpers
```

## Imports

```nanyx
import models/user
import utils/helpers

pub fn main() {
  let u = user.new("Alice")
  helpers.format(u)
}
```

## Qualified vs Unqualified Imports

```nanyx
// Qualified (recommended)
import nanyx/list
list.map([1, 2, 3], fn(x) { x * 2 })

// Unqualified specific items
import nanyx/list.{map, filter}
map([1, 2, 3], fn(x) { x * 2 })
```

## Aliasing

```nanyx
import some/deeply/nested/module as mod

mod.do_thing()
```

## Access Control

- `pub fn` — accessible from other modules
- `fn` — private to the current module
- `pub type` — type and constructors are public
- `pub opaque type` — type is public, constructors are private

```nanyx
pub opaque type Email {
  Email(String)
}

pub fn new(value: String) -> Result(Email, String) {
  case string.contains(value, "@") {
    True -> Ok(Email(value))
    False -> Error("Invalid email")
  }
}
```
