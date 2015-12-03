---
title: Nexus Configuration Server
platform: Javascript
github: etcinit/nexus
---

A simple web server for storing configuration and log files on a centralized
location with an API for use by other services.

<!--more-->

The concept behind Nexus is avoiding having to provide 10+ environment variables
when launching a Docker container. With Nexus, you only need two (The server
address and a token).

Nexus is currently in use by some services at SellerLabs.
