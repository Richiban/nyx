// Module: TestChaining
const value = 10;
const double = (x) => (x * 2);
const addFive = (x) => (x + 5);
const square = (x) => (x * x);
const chain1 = double(value);
const chain2 = addFive(double(value));
const chain3 = addFive(double(double(3)));
const chain4 = double(square(2));