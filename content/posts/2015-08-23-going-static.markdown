---
title: Dropping everything and going static
date: Mon Nov 30 04:56:00 EDT 2015
author: Eduardo Trujillo
uuid: 8a2cd1e5-edc9-46fc-82c0-739ace40f6ff
---

The last time I wrote a post about my blog was around two years ago.
At that type, I was riding on the Node.js hype train, so I was excited
to rewrite the engine my blog was running on in JavaScript.

Fast forward to today, and now this blog is just a bunch of statically
generated HTML files and images served by an Nginx server.

_So what happened?_

**Side projects. Lots of them.**

My blog used to be the _go-to_ hobby project to work on after school.
It served as a platform for me to learn about different languages,
security practices, and how to structure web applications.

Though, once I got my CS degree, I started working a full-time
developer, and, as part of the job, I started learning a lot and 
writing libraries to make said job easier.

After a while, I had many _go-to_ projects that I could work on during
my free time. Working on a project now meant also considering the
opportunity cost of not working on other projects, and the blog
routinely kept loosing this battle.

However, every now and then, the blog got some attention. Let's take a
look:

**Timeline:**

- _Pre-2014:_ Create a CMS from scratch in PHP and use it run a blog 
and a few other sites.
- _2014:_ Jump on the node.js hype train and rebuild a simpler CMS in 
Javascript while keeping compatibility with the original database 
schema.
- _Late 2014-Early 2015_: Port the views to React.js code while also 
making the server use isomorphic Javascript.
- _2015:_ Extract the backend code into a separate Go API, while also 
building a Kubernetes cluster to run the front-end and backend 
containers, plus a small Redis instance for API rate-limitting.

Out of all the things I tried, there was one "feature" that I kept
pushing back: A UI to write articles. My blog might have had 
bleeding-edge JS, an API, and a solid infrastructure, but it still
lacked content.

Don't get me wrong; These were really fun to implement and set up, and
I learned things that I later applied to other projects. However, the
high friction (posting articles over MySQL) kept getting in the way of
content, which is kind of the centerpiece.

Given the reduce availability, I decided to knock down the walls
obstructing content, and going for something simpler.

## Enter: Hakyll

**Jekyll** is a very popular tool for generating static sites. It is 
used by GitHub pages and many blogs out there. However, it is not the
only one: There's **Octopress**, **Hugo**, **GitBook**, and a [lot][1]
more.

One that caught my eye in particular was [Hakyll][3]. It is a very
similar tool, written in _Haskell_, a purely functional language.

Let's also mention that it's 2015, and that there's another hype train 
going around. This time its about Functional Programming.

So, I decided "if I'm going to create a statically-generated blog, why
not do it in style?". I jumped on another train and started writing my
new blog using Hakyll.

## feelsgoodman.jpg

Hakyll did not disappoint. It is a _really_ fast alternative to Jekyll
that also happens to be very extensible using plain Haskell code. This
were my favorite bits on working with it:

- You are not bound to just creating a blog, you can customize the
compilation process to create many different kinds of collections and
pages.
- It backed by [Pandoc][4], meaning that it supports reading posts in 
many file types, and also rendering them in other formats (like PDFs) 
besides just HTML.
- It's a Haskell script that gets compiled into an executable!
- It does not require you to know too much about functional programming
concepts.

<div class="callout-quote">
  Hakyll is simple and fast, but it's speed and extensibility are the 
  killer features.
</div>

In less than a week of working with Hakyll, I had a new blog setup 
with my previous posts, a projects mini-wiki, and a sweet new design.
However, this is probably because I already had some experience with
Haskell and the Gulp/SCSS combo.

If you want to take a peek under the hood, the code used for generating
this blog can be found on my [Phabricator instance][5]. I'm hoping to
write a follow-up post on some of the custom snippets in it.

### By the way:

- Always keep your posts in a portable format (like Markdown)! It will
make it easier to migrate your posts to other blog engines or static
site generators.
- If you are choosing which static site generator to use, make sure it
has good support for syntax highlighting.
- If you are trying out Hakyll, consider using [stack][2] instead of
just cabal. You can easily setup a Hakyll project by using their
template.
- Support for LaTeX is also a good thing to look for: 
$1 + 2 - \Omega = x + y + z^2$

[1]: https://www.staticgen.com/
[2]: https://github.com/commercialhaskell/stack
[3]: http://jaspervdj.be/hakyll/
[4]: http://pandoc.org/
[5]: https://phabricator.chromabits.com/diffusion/B/
