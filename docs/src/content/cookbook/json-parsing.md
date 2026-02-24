---
title: "Parse JSON Data"
description: "Working with JSON in Nanyx"
order: 2
---

# Parse JSON Data

Nanyx provides a `json` module for encoding and decoding JSON.

## Decoding JSON

```nanyx
import nanyx/json
import nanyx/io
import nanyx/dynamic

type User {
  User(name: String, age: Int)
}

fn decode_user(data: String) -> Result(User, json.DecodeError) {
  json.decode(data, {
    dynamic.decode2(
      User,
      dynamic.field("name", dynamic.string),
      dynamic.field("age", dynamic.int),
    )
  })
}

pub fn main() {
  let data = "{\"name\": \"Alice\", \"age\": 30}"
  
  case decode_user(data) {
    Ok(user) -> io.println("Hello, " <> user.name <> "!")
    Error(_) -> io.println("Invalid JSON")
  }
}
```

## Encoding JSON

```nanyx
let json_string = json.to_string(
  json.object([
    #("name", json.string("Alice")),
    #("age", json.int(30)),
    #("active", json.bool(true)),
  ])
)
// {"name":"Alice","age":30,"active":true}
```

## Working with Arrays

```nanyx
let users_json = json.to_string(
  json.array(users, fn(user) {
    json.object([
      #("name", json.string(user.name)),
      #("age", json.int(user.age)),
    ])
  })
)
```
