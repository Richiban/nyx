// Module: one
const message = "Hello world";
(() => { const __dbg = message; console.log(__dbg); return __dbg; })();
const gt5 = (x) => (x > 5);
const passes = dbg(gt5(6));
const myTag = { tag: "some", value: "data" };
const s = "";
const getName = (x) => x.name;
const x = { a: "a", b: "b", c: "c" };
const b = (() => {
  const _match0 = x;
  if (_match0 && typeof _match0 === "object" && "a" in _match0 && _match0.a === "a" && "b" in _match0 && _match0.b === "b" && "c" in _match0) {
    const c = _match0.c;
    return (() => {
  (() => { const __dbg = c; console.log(__dbg); return __dbg; })();
  c;
})();
  }
  else if (true) {
    return (() => { const __dbg = "something else"; console.log(__dbg); return __dbg; })();
  }
  throw new Error("Match failed");
})();
(() => { const __dbg = b; console.log(__dbg); return __dbg; })();
const items = [1, 2, 3];
const map = (a, b) => [];
const items2 = map(items, (x) => (x + 1));
const p = { name: "Alice", age: 30 };
dbg(getName(p));