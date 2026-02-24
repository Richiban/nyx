---
title: "Build a CLI Tool"
description: "Create a command-line application"
order: 1
---

# Build a CLI Tool

Let's build a simple CLI tool that counts words in a file.

## Project Setup

```bash
nanyx new word_counter
cd word_counter
```

## The Code

```nanyx
import nanyx/io
import nanyx/file
import nanyx/string
import nanyx/list
import nanyx/result

pub fn main() {
  case file.read("input.txt") {
    Ok(content) -> {
      let words = content
        |> string.split(" ")
        |> list.filter(fn(w) { w != "" })
      
      let count = list.length(words)
      io.println("Word count: " <> int.to_string(count))
    }
    Error(err) -> {
      io.println("Error reading file: " <> file.error_to_string(err))
    }
  }
}
```

## Run It

```bash
echo "hello world from nanyx" > input.txt
nanyx run
# Word count: 4
```

## Key Concepts

- **File I/O** — the `file` module returns `Result` types for safe error handling
- **Pipelines** — chain transformations with `|>` for readable data processing
- **Pattern matching** — handle success and failure cases explicitly
