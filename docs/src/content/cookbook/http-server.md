---
title: "Simple HTTP Server"
description: "Build a basic web server"
order: 3
---

# Simple HTTP Server

Build a basic HTTP server with Nanyx using the `nanyx_http` package.

## Setup

```bash
nanyx new my_server
cd my_server
nanyx add nanyx_http
```

## The Code

```nanyx
import nanyx/io
import nanyx_http/server
import nanyx_http/request.{Request}
import nanyx_http/response.{Response}

pub fn main() {
  let handler = fn(req: Request) -> Response {
    case request.path(req) {
      "/" -> response.text(200, "Welcome to Nanyx!")
      "/hello/" <> name -> response.text(200, "Hello, " <> name <> "!")
      _ -> response.text(404, "Not Found")
    }
  }

  io.println("Server running on http://localhost:3000")
  server.start(handler, on_port: 3000)
}
```

## Run It

```bash
nanyx run
# Server running on http://localhost:3000
```

## Key Concepts

- **Pattern matching on routes** — clean, readable routing without a framework
- **String pattern matching** — extract path parameters with `<>`
- **Immutable request/response** — functional approach to HTTP handling
