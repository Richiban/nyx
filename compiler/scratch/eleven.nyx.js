const dbg = (x) => { console.log(x); return x; }
const ctx = { println: (s) => dbg(s) };
const main = () => (() => {
  let __ctx = {};
  __ctx = { ...__ctx, ...ctx };
  return println("Hello world");
})();
main();