Rust implmentation of an interpreter for the Monkey programming language.  Following the book [Writing an Interpreter in Go](https://interpreterbook.com/).

Below is an example of the Monkey Language:

```JavaScript
let myVar = 1
myVar;

let age = 1;
let name = "Monkey";
let result = 10 * (20 / 2);

let myArray = [1, 2, 3];

let fibonacci = fn(x) {
      if (x == 0) {
          0
      } else {
          if (x == 1) {
              1
          } else {
            fibonacci(x - 1) + fibonacci(x - 2);
          }
      }
  };
  
let twice = fn(f, x) {
  return f(f(x));
};

let addTwo = fn(x) {
  return x + 2;
};

twice(addTwo, 2); // => 6
```
