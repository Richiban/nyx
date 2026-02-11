const dbg = (x) => { console.log(x); return x; }
// Module: one
const message = "Hello world";
dbg(message);
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
  dbg(`c = ${c}`);
  return c;
})();
  }
  else if (true) {
    return dbg("something else");
  }
  throw new Error("Match failed");
})();
dbg(`b = ${b}`);
const items = [1, 2, 3];
const map = (a, b) => [];
const items2 = map(items, (x) => (x + 1));
const p = { name: "Alice", age: 30 };
dbg(getName(p));