const f = (arg1) =>
  (() => {
    const _match0 = arg1;
    if (_match0 === 1) {
      return "one";
    } else if (true) {
      return "many";
    }
    throw new Error("Match failed");
  })();
const g = (__ctx) => {
  const { add, println } = __ctx;
  return (s) =>
    (() => {
      const r = add(4, 5);
      println(s);
      println(r);
    })();
};
const h = () =>
  (() => {
    let __ctx = {};
    __ctx = { ...__ctx, ...{ println: (s) => console.log(s) } };
    __ctx = { ...__ctx, ...{ add: (x, y) => x + y } };
    const { println } = __ctx;
    g(__ctx)("Hello world");
  })();
h();
