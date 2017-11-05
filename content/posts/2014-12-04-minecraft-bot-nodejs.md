---
title: Minecraft Bot in Node.js
tags: minecraft, nodejs, ai
uuid: a1efd1b4-abe3-41e6-af01-ab3d584f18d2
legacy: minecraft-bot-in-nodejs
---

I've recently been working on a Minecraft bot built in Node.js as part of my final project for my artificial intelligence class. The Minecraft-Node connection is possible thanks to a library called __Mineflayer__ which acts as a Javascript bridge between Minecraft and Node.

So far the bot can do the following things:

- Stare silently at the last player who talked in the chat
- Pick up dirt blocks from a pre-set resource area
- Navigate around (using the Mineflayer-Scaffold plugin)
- Keep track of state using a Knowledge Base
- Respond to events through logic rules
- Execute tasks by priority and dependencies (Example: Go to resource site before digging, or prioritize fighting back from digging)

The final goal is to have the bot build a simple column it can climb at night so it can survive longer than a day, and possibly have it build other things during the day.

### Demo:

<iframe width="480" height="360" src="https://www.youtube.com/embed/6Fk0kv0LM3g?rel=0" frameborder="0" allowfullscreen></iframe>

### Source code

Test it yourself or make your own changes: The source code is available at http://github.com/etcinit/minebot. It requires Minecraft 1.8 (not lower or higher) to work.
