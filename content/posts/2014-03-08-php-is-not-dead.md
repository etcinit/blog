---
title: 'Using Laravel: PHP is not dead'
tags: php, laravel
uuid: 9b92d244-281c-401c-8c72-3e9a67e976c3
legacy: using-laravel-php-is-not-dead
feature-image: https://i.imgur.com/p3m7kce.png
---

My last post in web development was about Node.js and how easy I found to build web apps with it. However, this doesn't mean I have abandoned PHP, in fact, it has pushed me to __write better PHP applications and improve the quality of my code__.

## Old vs New PHP

A common thing that has happened to me recently is that when I tell other CS majors (at least the ones that know about web development) that I write web applications in PHP, they instantly start talking about how awful it is as a language, how antiquated it is, etc.

These claims are not entirely baseless; PHP has seen better days, and many parts of the language have always felt like a hack due to the lack of standards and inconsistent type system (I can declare the type of a parameter of a function as class but not a string or integer?).

However, PHP hasn't stayed frozen in time, the community has slowly been improving the language by adding some cool features.

Sadly, __the general consensus in some cases still seems to be that applications in languages like Java and PHP are old and hard to write__, and that you as developer are uncool if you are not using a *"hip and modern"* framework/language like Ruby on Rails or Node.js.
In addition to the regular need to improve software, this situation has pushed the PHP community to improve their practices and have a modern development workflow. Sort of like a __?We can do modern development too?__, which shows that the community is resilient and that it is open to major changes.

<blockquote class="twitter-tweet" lang="en"><p>Chris Spruck (<a href="https://twitter.com/cspruck">@cspruck</a>) delivers the announcements to a packed house at <a href="https://twitter.com/AtlantaPHP">@AtlantaPHP</a> earlier tonight. <a href="http://t.co/yleFZQwh9u">pic.twitter.com/yleFZQwh9u</a></p>&mdash; Ben Ramsey (@ramsey) <a href="https://twitter.com/ramsey/statuses/441803542891081729">March 7, 2014</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

A few days ago I attended an [AtlantaPHP](http://atlantaphp.org) talk on modern PHP development, by [Ben Ramsey (@ramsey)](https://twitter.com/ramsey), and some of the ideas that got my attention were that the PHP community is very good at ?borrowing? concepts and including them into the language and frameworks, and that PHP development is not only about the language itself, but the tools around it and they way applications are built.

One of the most important recent projects in PHP is Composer. Composer is a very similar to other package managers like Node's npm. It greatly simplifies the process of sharing and including external libraries into a project (Specially since code can be easily hosted online in places like Bitbucket and Github). This puts PHP's workflow closer to how you would build a Node.js application.

The old way I used to write PHP applications consisted on me trying to build everything from scratch (the *?not built here syndrome?*), which resulted in big projects that were difficult to maintain. I even got as far as to build my own PHP bsic framework (See: [Brickwalls CMS](https://bitbucket.org/eduard44/brickwalls-2)) which attempted to organize applications into a basic MVC model.

Being able to use multiple modules from different libraries put some on the weight of developing and maintaining an application on the community rather than all of it on yourself or your team. Many PHP open source libraries are well maintained and probably more secure than your own code (Although it's always good to look at the code that you're importing into your project).

Additionally, just because somebody writes an application in PHP it doesn't mean that they are barred from using tools like Grunt, Bower or Rubygems even if these are written in another language or platform. Specially since CSS pre-processors like SASS and LESS, and Javascript minification and dependency resolution are becoming standard in many applications nowadays.

## A modern PHP framework

![Logo](https://i.imgur.com/p3m7kce.png "Logo")

The other big thing in PHP right now are frameworks. There are a bunch of them, and thanks to standards and Composer they are all cooperating together. My favorite one so far is Laravel.

Development in Laravel is much different from classic PHP development, and the best part is that it achieves this by changing how we build applications rather than the syntax of the language.

I like to think that Laravel as __a big compilation of web development patterns and knowledge__ that people have gained over time (also in part because it includes many smaller libraries as dependencies). It provides libraries for most basic development tasks and application structure, such as a database abstraction system and a request router. Additionally, if you ever need a library that is not included it is pretty easy to include it using Composer.

Something that kept me away from libraries for a long time was the fact that many frameworks force you into molding your code and mind to fit their way of thinking. This leads to developers fighting with code or frustrated because the framework may not expressive enough or do some things in a weird way.
Laravel is very different in this aspect thanks to the fact that most of it's components are optional and have very standard behaviors. While you won't probably be doing things different form the Laravel way for basic applications, you still have enough flexibility to implement custom behaviors just by ignoring components or using a lower level library (like using a Query Builder instead of a ORM like Eloquent).

Similarly to how Ruby on Rails works, Laravel also includes a command line tool called Artisan for performing common tasks such as setting up the database or creating a simple PHP development server __(Yes, no Apache installation required)__. It also is extensible, so you can use tools like Laravel Generators for creating boilerplate code like Controllers, Views, Models and Routes. In fact, you can get a basic prototype web application just by using Artisan command tools.

In general, I have enjoyed using Laravel a lot recently, and it really has helped me on an internship project I'm currently working on. It definitely restores my hope on PHP and makes me want to write more applications on PHP due to its simplicity and maturity. __If you want to try PHP, I definitely recommend using Laravel.__
