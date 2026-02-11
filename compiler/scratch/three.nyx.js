const dbg = (x) => { console.log(x); return x; }
const f = (x, y) => (() => {
  return "";
})();
const args = { x: 4, y: 5, z: 0 };
f({ x: 4, y: 5 });
f(args);
const u = { tag: "nil" };
const g = (o) => (() => {
  return "";
})();
g(u);
const mapOption = (arg1, arg2) => (() => {
  const _match0 = arg1;
  const _match1 = arg2;
  if (_match0 && typeof _match0 === "object" && _match0.tag === "some") {
    const v = _match0.value;
    const f = _match1;
    return { tag: "some", value: f(v) };
  }
  else if (true) {
    const other = _match0;
    return other;
  }
  throw new Error("Match failed");
})();
const l = { tag: "nil" };
const l2 = mapOption(l, (s) => 1);
dbg(l2);