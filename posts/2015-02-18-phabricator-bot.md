---
title: Building a Phabricator and Slack bot
tags: phabricator, slack, nodejs
uuid: 75330309-7fa2-4ea3-96f9-6f67127fe744
---

As the result of a short hacking session, I built a simple Node.js bot that forwards items from the Phabricator news feed into a Slack channel:

![Example](http://i.imgur.com/128Gkjw.png)

Phabricator supports calling POST endpoints whenever there is a new feed item (setup through the `feed.http-hooks`). However, we can't just forward these directly to Slack. The main task of the bot is to take these requests from Phabricator, fetch some extra information using Conduit and the calling another webhook to deliver the message to a Slack channel.

Right now it seems to be working fine with most object types (tasks, commits, wiki pages). The only side-effect is that the bot will cause Slack notifications appear for your own actions if your username in Slack and Phabricator happen to match.

You can find the code and instructions on how to set it up on [GitHub](https://github.com/etcinit/phabricator-slack-feed)
