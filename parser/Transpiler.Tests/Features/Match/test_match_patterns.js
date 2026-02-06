// Module: MatchPatterns
const withGuard = (() => {
  const _match0 = value;
  if (_match0 > 5) {
    return 1;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
const listMiddle = (() => {
  const _match0 = values;
  if (Array.isArray(_match0) && _match0.length >= 2 && _match0[0] === 1 && _match0[_match0.length - 1] === 3) {
    return 1;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
const recordMember = (() => {
  const _match0 = point;
  if (_match0 && typeof _match0 === "object" && "x" in _match0 && _match0.x === 1 && "y" in _match0) {
    const y = _match0.y;
    return y;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
const recordPositional = (() => {
  const _match0 = point;
  if (_match0 && typeof _match0 === "object" && _match0[0] === 1) {
    const y = _match0[1];
    return y;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
