const dbg = (x) => { console.log(x); return x; }
const printContext = { println: (s) => dbg(s) };
const f = (__ctx) => { return (s) => (() => {
  __ctx.println("Printy print:");
  return __ctx.println(s);
})(); };
const main = () => (() => {
  let __ctx = {};
  __ctx = { ...__ctx, ...printContext };
  return f(__ctx)("Hello world");
})();
main();