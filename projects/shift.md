---
title: Shift
platform: Haskell
github: etcinit/shift
uuid: 58e8fe22-59a1-462a-8a9e-c238a4fd3da5
---

A changelog generator.

<!--more-->

Born out of the frustration that comes out of having to manually update
changelog files, Shift is a tool that can automatically generate a change log
based on a project's Git history.

To be ultimately useful, Shift does require that the project follows a git
commit convention. In it's initial implementation, it solely supports the
Angular commit convention since it is one of the most popular of these kind of
conventions and fairly easy to parse.

I chose Haskell to write this project due to its very powerful and declarative
parsing libraries, which are use extensively to parse commit messages and
bodies.

Additionally, Shift takes advantage of projects hosted on GitHub and can
generate richer changelogs by embedding links to commit diffs and author
profiles.
