import fs from 'node:fs/promises';
import path from 'node:path';
import wabtFactory from 'wabt';

const watFileArg = process.argv[2] ?? './twelve.wat';
const exportName = process.argv[3] ?? 'main';
const callArgs = process.argv.slice(4).map((value) => Number.parseInt(value, 10));

if (callArgs.some(Number.isNaN)) {
  console.error('All function arguments must be integers.');
  process.exit(1);
}

const watPath = path.resolve(process.cwd(), watFileArg);
const watSource = await fs.readFile(watPath, 'utf8');

const wabt = await wabtFactory();
const parsedModule = wabt.parseWat(watPath, watSource);
const { buffer } = parsedModule.toBinary({ log: false, write_debug_names: true });

const imports = {
  env: new Proxy(
    {
      dbg(value) {
        console.log(`[dbg] ${value}`);
      }
    },
    {
      get(target, prop) {
        if (typeof prop !== 'string') {
          return target[prop];
        }

        if (prop in target) {
          return target[prop];
        }

        if (prop.startsWith('dbg_tag_')) {
          const suffix = prop.slice('dbg_tag_'.length);
          return () => {
            console.log(`[dbg] #${suffix}`);
          };
        }

        return undefined;
      }
    }
  )
};

const { instance } = await WebAssembly.instantiate(buffer, imports);
const exported = instance.exports[exportName];

if (typeof exported !== 'function') {
  console.error(`Export '${exportName}' is not a callable function.`);
  process.exit(1);
}

const result = exported(...callArgs);
if (result !== undefined) {
  console.log(`[result] ${result}`);
}
