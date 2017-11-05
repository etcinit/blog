---
title: Blog
platform: Haskell
github: etcinit/blog
post: /posts/2016/02/15/serving-hakyll-site-with-warp/
uuid: 05ba78e4-3659-4f39-9afc-ac50868f081c
---

The generator and server behind this blog.

<!--more-->

**About**

The Chromabits blog has gone over multiple engines over time. A long time ago,
the site was powered by a dynamic PHP CMS. After a while, it migrated to a Node
JS server with isomorphic JS. Finally, it has recently been reborn as a static
website generated using the Hakyll library/framework.

[_Dropping everything and going static_][1] describes this in greater detail.

**Architecture**

The project could be subdivided into three main sections: The generator, the
server, and the frontend.

The generator uses Hakyll, which then uses Pandoc, to take many posts and
project descriptions written in Markdown into full pages using different
templates and some basic logic (like the random title colors).

The server uses the [kawaii][2] package to define a custom web server that took
less time to program and setup than the trial and error that setting up Apache
through configuration files can be sometimes.

The frontend is simply a Gulp task for compiling SASS into a minified CSS file.

[1]: /posts/2015/08/23/going-static/
[2]: /projects/kawaii/
