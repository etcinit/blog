---
title: Speedbump
platform: Go
github: etcinit/speedbump
sourcegraph: https://sourcegraph.com/github.com/etcinit/speedbump
docs: https://godoc.org/github.com/etcinit/speedbump
---

A rate-limitter in Go.

<!--more-->

Uses Redis as its backing store to keep track of requests per client and properly apply limits through a cluster of servers. While it includes timing functions for per second, minute and hour limits, it also supports custom functions.

Additionally, a middleware component for the popular Go framework, Gin is included. Making it easier to add rate limits to APIs and web applications written in Go and running behind a load balancer (such as Amazon's ELB).
