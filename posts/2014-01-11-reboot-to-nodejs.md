---
title: Reboot to Node.js
tags: nodejs, javascript, review
uuid: b21f4eeb-c2f7-4194-b17c-5699ef4620cb
legacy: reboot-to-nodejs
---

So Node.js has been around for while now and if you read blogs about web development, you probably have heard how good it is supposed to be. For the past few years I have gotten pretty comfortable with PHP, and as a result things got a little bit uninteresting or slightly boring. So I decided to try out and learn a new language for web development. I was specifically looking to learn a language that allows you to roll your own web server. I was already learning about Java web dev in one of my classes that semester (JBoss specifically) so my two remaining options were Python and Node.
I ended up choosing Node due to the fact that it used JavaScript (which I had done a pretty good job at avoiding so far) which is also used by most modern web browsers. My first project and learning experience consisted on rewriting the code that runs the blog you are reading right now. Along the way, I found a couple of things that I liked and a few that I didn?t like. Here are some of them:

## Liked:

__Packages and code reuse:__ This is a biggie. For me, it felt like Node was designed with the intention of code reuse. The abundance of packages that do all sort of things really allow developers to focus on building applications rather than forcing them reinvent the wheel every time. PHP has had things like Composer but I don?t think the number of packages available even compares.

__Simplicity:__ I found JS to be a very simple and easy to understand language. There are a few things that I?m still trying to get the grasp of (like creating objects and working with prototypes), but in general I think that it was very easy to pick up and many of the functions included by Node are very well documented.

__?Owning? the server:__ This is something I see as just more efficient (I could be wrong). The fact that applications run continuously and create their own HTTP server, and in general keep a lot of things already loaded in memory seems more efficient to me. For example, a template engine I was using keeps precompiled versions of each template in memory in order to avoid loading them on every request. In comparison, PHP on Apache has to load everything from scratch every time a user makes a request.

__Source-code and HTML are separate:__ This one has a lot to do with PHP. I really like having separate files for my code and HTML.

## Didn?t like:

__Messy code and legibility:__ This became apparent as my application grew in complexity, but it might be more related to the fact that I?m a beginner rather than the language itself. Once you have a couple of functions nested inside of each other, things can get a little messy and hard to read.

In general, Node seems like a great language for rapid prototyping and small applications. Its package system makes it perfect for projects that mix a lot of technologies together. Although, I would probably need to learn better JS coding patterns or consider other options like Java or Python if I was to build a more complex application.
