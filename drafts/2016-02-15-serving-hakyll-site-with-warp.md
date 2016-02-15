---
title: Serving a Hakyll site with Warp/Wai
date: Mon Feb 15 02:00:00 EDT 2016
author: Eduardo Trujillo
uuid: 16b3b144-0fb7-422c-a31e-0fca75e28206
---

Working with Hakyll has been great so far, but it seems the fun ends right after your site is generated and copied over to the remote server. From there, you have to deal with Nginx or Apache configuration files to give the final touches to the site, such as redirections, cache policies and additional headers.

I've seen applications, such as XMonad, which employ Haskell scripts in-lieu of configuration files, and it's generally an enjoyable experience due to the added flexibility and customization it provides.

With that in mind, wouldn't it be nice if the web server was in Haskell as well? After all, Hakyll has a preview function that works well enough. So it shouldn't be too hard to replicate that and add some custom logic on top of it.

Well, that's exactly what I was wondering in the past few days, and (perhaps not too) surprisingly, it only took half a day's worth of work to get a working prototype.

To get the job done, I added a new target to my Hakyll project, and included Warp, wai-static-app, and a bunch of WAI middleware as dependencies. The rest was merely a process of putting all the pieces together and ensuring the site behaved as expected.

## The result

* A fast static site server.
* Support for HTTP 2.0
* Customized configurations for production, staging, and development.
* A more complete solution than `'hakyll preview`, since the site behaves exactly like it would on production.
* A clear path for adding more logic in the future (API, Proxy, etc).

The server code is a few orders of magnate larger than a simple configuration file, but it's also under 200 lines of code.

> server code

## Lets dive in!

Now let's dive into the details:

### forkIO

forkIO was one of the most interesting things I encountered while writing the server, and it also one of the last.

If you are writing a server to run on a single port, you don't generally stop and ask yourself how you would modify it to run on multiple port simultaneously.

Well, if you expect your site to use TLS and HTTP 2.0, you will most likely want to also redirect users from the insecure site to the secure one. This means you will need to listen on both, ports 80 and 443.

My initial approach to this was very naive. I concluded that I could simply  just run two servers by passing a flag at startup. However, this seemed cumbersome and error-prone. 

The real solution lead to learn a new part of Haskell that I had largely ignore before: Concurrency.

I was please to find that `forkIO` allows you to create a lightweight thread that can execute another `IO ()` function.

With it, I was able to spawn both servers at the same time:

> Example

