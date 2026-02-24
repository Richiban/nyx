const dbg = (x) => { console.log(x); return x; }
const main = () => (() => {
  const _await = (arg1, arg2) => (() => {
  const _match0 = arg1;
  const _match1 = arg2;
  if (_match0 && typeof _match0 === "object" && _match0.tag === "pending") {
    const v = _match0.value;
    const cont = _match1;
    return cont(v);
  }
  else if (_match0 && typeof _match0 === "object" && _match0.tag === "done") {
    const v = _match0.value;
    const cont = _match1;
    return cont(v);
  }
  throw new Error("Match failed");
})();
  const fromResult = (r) => ({ tag: "done", value: r });
  const a = fromResult(5);
  return _await(a, (b) => (() => {
  const c = (b + 1);
  return (() => {
  dbg(c);
  return undefined;
})();
})());
})();
main();