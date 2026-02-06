// Module: TestTags
const none = { tag: "None" };
const someInt = { tag: "Some", value: 42 };
const someStr = { tag: "Some", value: "hello" };
const ok = { tag: "Ok", value: 100 };
const err = { tag: "Error", value: "failed" };
const nested = { tag: "Result", value: { tag: "Some", value: 42 } };