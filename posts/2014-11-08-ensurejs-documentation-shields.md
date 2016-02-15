---
title: Ensure 0.5.0: Documentation and shields!
tags: ensure, nodejs
uuid: a4d02358-7410-46ed-be8a-b99730a0e45d
legacy: ensure-050-documentation-and-shields
---

_Ensure.js is my JavaScript dynamic type checking library. For more information go [here](https://chromabits.com/post/ensurejs-simple-type-checking-on-javascript)_

It's been a while since I worked on Ensure.js, but I finally got around polishing and writing new features. In this release you can find the following:

## New documentation

![Documentation screenshot](https://i.imgur.com/GmUC1PE.png)

Most of the library is now documented using JSdoc. Most functions should have explanations of their behavior and some of them have usage examples.

The documentation for the latest version will be online at: [http://assets.chromabits.com/ensure/docs/](http://assets.chromabits.com/ensure/docs/ensure.html) and it is also included with the source code.

## Shields: Protect your functions

One of the most common use cases for Ensure.js that I've encountered is checking the input arguments of a function. However, adding a bunch of `ensure` statements at the beginning of each function can get a bit repetitive after a while.

Version 0.5.0 now comes with a factory function called Shield (`ensure.shield`), which allows you to add a simple wrapper around your function that will type check your arguments and your return values automatically for you:

```javascript
// First we create our shielded function
var myFunction = ensure.shield(
	[Boolean, Array],
	Number,
	function (arg1, arg2) {
		if (arg1 === true) {
			return {};
		}

		return 1337;
	}
);

// This works fine
myFunction(false, [1, 2]);

// This throws an error since argument 1 is not a Boolean
myFunction([1, 2], [3, 4]);

// This throws an error since it does not return a Number
myFunction(true, [1, 2, 3]);
```

## Production mode

This is a feature I've been thinking about ever since the first version of Ensure.js came out: Dynamic type checking can be heavy on your application if you use them extensively. However, once your application is in production, there shouldn't been a need for type checking (unless your parsing some unknown input or server response).

So, Ensure.js now comes with a global variable for disabling type checks:

```javascript
ensure.enforce = false;

// This will not throw an error
ensure('Hello', Array);

// This will always return true
ensure('Hello', Array, true);
```

However, I'm aware that this does not cover every single scenario. Maybe version 1.0 will include a way of instantiating Ensure so you can have multiple instances of the library with different configurations.

## Other changes

- [Breaking] Improved the behavior of `has()` functions
- More unit tests for various parts of the library
- Added `ensure.isNotIn()`

---

Source code for Ensure.js is available at GitHub:
[https://github.com/eduard44/ensure](https://github.com/eduard44/ensure) and it is available as a package on Bower and NPM as `ensure.js`

Please submit any bugs reports or feature requests at [https://github.com/etcinit/ensure/issues](https://github.com/etcinit/ensure/issues)
