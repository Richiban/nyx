// Module: MatchBasic
const byLiteral = (() => {
  const _match0 = 3;
  if (_match0 === 1) {
    return 10;
  }
  else if (_match0 === 3) {
    return 30;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
const byTag = (() => {
  const _match0 = value;
  if (_match0 && typeof _match0 === "object" && _match0.tag === "Some" && _match0.value === 42) {
    return 1;
  }
  else if (_match0 && typeof _match0 === "object" && _match0.tag === "None") {
    return 0;
  }
  else if (true) {
    return -1;
  }
  throw new Error("Match failed");
})();
