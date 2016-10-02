---
title: Craze
platform: Haskell
github: etcinit/craze
docs: https://hackage.haskell.org/package/craze
uuid: 427d241d-384a-4468-8b0e-11e0c71861b1
---

A micro-library for racing HTTP GET requests.

<!--more-->

Craze is my first open-source Haskell package. It started as a small algorithm
for work, but as gained more features it was polished enough to open-source.

**What is Craze?**

Craze is a small module for performing multiple similar HTTP GET requests in
parallel. This is performed through a function called `raceGet`, which will
perform all the requests and pick the first successful response that passes a
certain check, meaning that the parallel requests are essentially racing
against each other.

**What is the usefulness of this?**

If you are dealing with data source or API that is very unreliable (high
latency, random failures) and there are no limitations on performing
significantly more requests, then performing multiple requests (through direct
connections, proxies, VPNs) may increase the chances of getting a successful
response faster and more reliably.

However, if using a different data source or transport is a possibility, it is
potentially a better option that this approach.
