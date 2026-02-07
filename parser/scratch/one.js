// Module: one
const message = "Hello world";
const main = (x) => (() => {
  console.log(message);
  const gt5 = (x) => (x > 5);
  const passes = gt5(gt5(6));
  const myTag = { tag: "some", value: "data" };
  const s = "";
  const getName = (x) => x.name;
  const x = { a: "a", b: "b", c: "c" };
  const b = (() => {
  const _match0 = x;
  if (_match0 === "hi") {
    return "bye";
  }
  else if (_match0 === "one") {
    return "two";
  }
  else if (_match0 && typeof _match0 === "object" && "a" in _match0 && _match0.a === "a" && "b" in _match0 && _match0.b === "b" && "c" in _match0) {
    const c = _match0.c;
    return c;
  }
  throw new Error("Match failed");
})();
  console.log(b);
  const items = [1, 2, 3];
  const map = (a, b) => "";
  const items2 = map(items, (x) => x.name);
})();
main();