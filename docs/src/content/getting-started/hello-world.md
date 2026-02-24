---
title: "Hello, World!"
description: "Write your first Nanyx program"
order: 3
---

# Hello, World!

Let's create your first Nanyx project and run it.

## Create a Project

```bash
nanyx new hello_world
cd hello_world
```

This creates a new project with the following structure:

```
hello_world/
├── nanyx.toml
├── src/
│   └── hello_world.nx
└── test/
    └── hello_world_test.nx
```

## Your First Program

Open `src/hello_world.nx` and you'll see:

```nanyx
import nanyx/io

pub fn main() {
  io.println("Hello, World!")
}
```

## Run It

```bash
nanyx run
# Hello, World!
```

## Understanding the Code

- `import nanyx/io` — imports the I/O module from the standard library
- `pub fn main()` — declares the public entry point function
- `io.println(...)` — prints a line to standard output

## What's Next?

You're ready to explore the language! Head to the [Language Tour](/docs/tour/variables) to learn Nanyx step by step.
