---
title: Experimenting with structures
tags: php, datastructures, testing
uuid: 760eaf0c-585a-405e-b680-6d45343986f1
legacy: experimenting-with-structures
author: Eduardo Trujillo
---

For many programming languages and software packages, data structures such as linked lists and maps, and sorting algorithms like quicksort, are things that come built-in.

In terms of performance and working on higher levels, these built-in packages are a really good idea. In the case of languages like PHP, they are likely faster or better optimized than anything you could write in the language itself due to the internal being written in C.

However, when you want to look under the hood and understand a bit more what you've been learning in that _Algorithms & Data Structures?_ class, programming your own data structures can be both fun and satisfying.

So that's exactly what I ended up doing: Writing my own implementation of popular data structures and sorting algorithms in, oddly enough, PHP.

![](https://i.imgur.com/9Owjani.png)

PHP is like a sweet spot between Java and JavaScript. You get an interpreted language without strict typing, but with the added benefit of OOP features like Interfaces and Traits.

The project structure is simple: a bunch of classes neatly organized into namespaces according to the concept they represent, interfaces (for future expandability), and many unit tests to back everything up.

### Code quality

Working on this project also reassured me the importance of testing your code, but also of avoiding to repeating your self:

For this project I had decided to try a cool-looking site, CodeClimate, which promised to provide automated code review. Given that this is just a personal experiment and that their service is free for public repositories, I decided to give it a try.

After a few commits, their website pointed out the obvious: many of my unit tests had duplicated code. This led to think about ways of combining logic in unit tests, which is not something I have attempted to do before.

![](https://i.imgur.com/LgvktgJ.png)

Having setup a namespace for tests, which can be properly defined in Composer using the `require-dev` key, really helped in getting this done.

After identifying common logic and some refactoring, I had some abstract tests and inheritance setup, which removed most of the warnings on Code Climate, and made it simpler to write new similar tests.

### Complexity

Linked lists, Quicksort, Stacks, Insertion sort were all very simple and straightforward to implement, possibly thanks to the fact that they have been mentioned on many lectures back in school.

However, out of all the data structures I've worked on (at least at the time of writing), HashMap was the most complicated one.

I attribute this to the fact that a simple implementation of HashMaps require the use of another data structure to handle collisions and has to be able to grow and shrink depending on the load factor.

Unit testing, again, provided to be very useful since it allowed me to be sure that the HashMap class was working as I expected.

Using my testing approach, I was unable to directly test that the growing and shrinking of the HashMap's internal array was happening correctly. Given that these are internal operations of the data structure, there are no public methods for querying the status or altering its behavior, and adding methods just for the sake of code coverage seemed wrong, almost like cheating.

So the current tests mainly tries to simulate a real-world scenario in which the HashMap has to expand and contract multiple times, while making assurances that it still works and hasn't lost data.

### Looking ahead

So far this has been an interesting exercise, and I plan to keep adding more data structures/sorting algorithms in the future just to keep my coding skills sharp.

You can take a look at the code or fork it and play with it on GitHub: https://github.com/etcinit/structures

### Examples

Here are some usage examples:

```php
use Chromabits\Structures\Map\HashMap;

$map = new HashMap();

$map->set('hello', 'world');

$map->has('hello');
>> true

$map->get('hello');
>> 'hello'

$map->toArray();
>> ['hello' => 'world']
```

Basic quicksort algorithm comparing strings:

```php
use Chromabits\Sorting\Quicksort\QuicksortSorter;
use Chromabits\Sorting\Comparators\StringComparator;

$sorter = new QuicksortSorter(new StringComparator());

$sorter->sort(['z', 'g', 'c', 'a']);
>> ['a', 'c', 'g', 'z']
```
