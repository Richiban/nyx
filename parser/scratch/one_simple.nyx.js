// Module: one
const f = (x) => (() => {
  (() => {
  const _match0 = x;
  if (_match0 === "hi") {
    return "bye";
  }
  else if (_match0 === "one") {
    return "two";
  }
  throw new Error("Match failed");
})();
})();
const g = (arg1) => (() => {
  const _match0 = arg1;
  if (_match0 === "hi") {
    return "bye";
  }
  else if (_match0 === "one") {
    return "two";
  }
  throw new Error("Match failed");
})();
const f2 = (arg1) => (() => {
  const _match0 = arg1;
  if (_match0 && typeof _match0 === "object" && _match0.tag === "a") {
    return "got a";
  }
  else if (_match0 && typeof _match0 === "object" && _match0.tag === "b") {
    return "got b";
  }
  throw new Error("Match failed");
})();
const sum = (arg1) => (() => {
  const _match0 = arg1;
  if (Array.isArray(_match0) && _match0.length === 0) {
    return 0;
  }
  else if (Array.isArray(_match0) && _match0.length >= 1) {
    const head = _match0[0];
    const tail = _match0.slice(1);
    return 1;
  }
  throw new Error("Match failed");
})();
const main = () => (() => {
  (() => { const __dbg = sum([1, 2, 3]); console.log(__dbg); return __dbg; })();
})();
main();