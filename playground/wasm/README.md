# WASM Playground Runner

Run generated Nanyx `.wat` files locally with a tiny host runtime.

## Setup

```powershell
cd playground/wasm
npm install
```

## Run

```powershell
npm run run -- ./twelve.wat main
```

## Compile + Run Nanyx in One Command

```powershell
npm run run:nyx -- ./twelve.nyx main
```

With integer export arguments:

```powershell
npm run run:nyx -- ./math.nyx add 2 3
```

You can pass integer arguments after the export name:

```powershell
npm run run -- ./some-module.wat add 2 3
```

The host provides an `env.dbg(i32)` import that logs as:

- `[dbg] <value>`

If the export returns a value, it logs as:

- `[result] <value>`
