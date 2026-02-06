// Module: TestLambdas
const identity = (x) => x;
const add = (x, y) => (x + y);
const triple = (x) => (x * 3);
const useIdentity = identity(42);
const useAdd = add(5, 10);
const applyTwice = (f, x) => f(f(x));