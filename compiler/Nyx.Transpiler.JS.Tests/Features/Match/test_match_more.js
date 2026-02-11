// Module: MatchMore
const listEnd = (() => {
  const _match0 = values;
  if (Array.isArray(_match0) && _match0.length >= 2 && _match0[0] === 1 && _match0[1] === 2) {
    const rest = _match0.slice(2);
    return rest;
  }
  else if (true) {
    return [];
  }
  throw new Error("Match failed");
})();
const tupleCase = (() => {
  const _match0 = pair;
  if (Array.isArray(_match0) && _match0.length === 2 && _match0[0] === 1) {
    const x = _match0[1];
    return x;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
const rangeCase = (() => {
  const _match0 = value;
  if (_match0 >= 1 && _match0 <= 3) {
    return 1;
  }
  else if (true) {
    return 0;
  }
  throw new Error("Match failed");
})();
const tagInline = (() => {
  const _match0 = result;
  if (_match0 && typeof _match0 === "object" && _match0.tag === "ok") {
    const data = _match0.value;
    return data;
  }
  else if (_match0 && typeof _match0 === "object" && _match0.tag === "error") {
    const msg = _match0.value;
    return "Error: {msg}";
  }
  else if (true) {
    return "unexpected";
  }
  throw new Error("Match failed");
})();
const guardCase = (() => {
  const _match0 = value;
  if (_match0 > 10) {
    return "big";
  }
  else if (true) {
    return "small";
  }
  throw new Error("Match failed");
})();
