---
title: Kawaii
platform: Haskell
github: etcinit/kawaii
docs: https://hackage.haskell.org/package/kawaii
uuid: ea143dfb-4312-4afa-91fd-004ebd02fb3d
---

Utilities for serving static sites and blogs with Wai/Warp.

<!--more-->

Warp is a really performant web server for Haskell applications that follow the
Wai interface. Kawaii is a library that encapsulates a lot of the boilerplate
involved in setting up a Warp/Wai server for hosting static websites and blogs.

The library was originally part of the source code of the Chromabits blog, but
refactored to be more generic and work with more projects.

**Features:**

- Wai applications: Static website, home redirector.

- Built-in HTTP and HTTPS servers.

- A CLI for launching a basic server on any directory.

- Middlewares:

    - Logger: Logs all requests to `stdout` in Apache-like format.

    - Force SSL: Redirects all visitor accessing a site from HTTP to the HTTPS
      version.

    - GZip

    - Domain enforcer: Redirects visitors to a specified domain. This allows
      authors to create redirections from a www to a non-www domain or
      vice-versa.

    - Security headers

    - HSTS

    - Content Security Policies: Besides adding the appropriate headers, the
      middleware includes a mini-DSL for describing the desired rules in
      Haskell.

    - De-indexify middleware: Many static site generators use `index.html` for
      many paths to avoid showing any extensions as part of an URL. This
      middleware ensures that even if a visitor directly accesses a URL ending
      in `index.html`, they get redirected and served the directory version:
      `my.blog/index.html --> my.blog/`.
