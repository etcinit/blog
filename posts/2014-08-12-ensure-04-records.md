---
title: Ensure.js 0.4: Record Types and better testing
tags: type-checking, ensure, nodejs, javascript
uuid: e1b251c3-10cd-41d1-9d64-faa70d407e9b
legacy: ensurejs-04-record-types-and-better-testing
---

I just finished pushing a new set of updates to Ensure.js! Now all tests are run through the
mocha framework and cover about 99% of the code. However, the big news in this release
is the addition of record types.

## Record Types

Languages like C++, Hack and Haskell have nice syntax for data types that hold
information. You might know them as Structs, Shapes or Records.

I have been playing with both [Haskell](http://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_.28Record_Syntax.29)
and [Hack](http://docs.hhvm.com/manual/en/hack.shapes.php)
recently and I thought that it would be nice
to have something similar in JavaScript. So I decided to build upon the methods already provided
by Ensure.js and add Record Types (inspired from Haskell)

Ensure.js provides an
emulation of this format. I say emulation because it is not analyzed
statically like other languages and they might have some behavior differences.

However, it can still be useful
for adding some validation to certain objects in your application. Instead of
blindly assuming that a certain variable of an object is of a certain type
you can use Ensure Records which automatically perform these checks for you.

_Please note that if performance is important, it is always faster to just use
regular objects due to the fact that all checks are performed at runtime._

## New methods:

__EnsureRecord(spec)__

*spec*:
The spec is an object specifying the types of each variable in the record, where
the key is the name of the property and the value the type to expect.

__EnsureRecordType(values)__: (Name depends on the name given to the type, see example)

*values*:
The values being applied to the instance being instantiated. Will throw an error if
the types do not match the spec

### Example:

```js
var ensure = require('ensure'),
    EnsureRecord = ensure.EnsureRecord;

// First, we create our person type
var Person = new EnsureRecord({
        firstName: String,
        lastName: String,
        age: Number
    });

// Then we create an instance by providing its values
var bob = new Person({
        firstName: "Bob",
        lastName: "Lulz",
        age: 20
    });

console.log(bob.firstName)
>>> "Bob"

// Note that if we try to brake the spec, we get an error
var alex = new Person({
        firstName: "Bob",
        lastName: "Lulz",
        age: "Old"
    });
>>> [TypeException]

// Same with setters:
bob.name = [1, 5, 7];
>>> [TypeException]
```

Hopefully this will help with writing or debugging with your application. The next release goal
is to add different modes ("strict", "production") that can be set through a settings object.

As usual, the code is on github: https://github.com/etcinit/ensure

_Update: Version 0.4.0 contained a bug when using multiple record properties (like in the example).
This bug is now fixed on version 0.4.1_
