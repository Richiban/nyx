// Module: TestBlocks
const blockSimple = (() => {
  const x = 1;
  x;
  const blockTwoStmts = (() => {
  const x = 1;
  const y = 2;
  (x + y);
  const blockNested = (() => {
  const outer = 10;
  const inner = (() => {
  const nested = 20;
  (nested * 2);
  (outer + inner);
})();
})();
})();
})();