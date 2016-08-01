---
title: 'Linux setup: Gnome 3'
tags: linux, gnome, fedora
uuid: 97473bd1-82a8-412a-8863-68ca30a5ef6e
legacy: linux-setup-gnome-3
---

[![Desktop screenshot](/img/posts/gnome/screenshot.png)](/img/posts/gnome/screenshot.png)

*I have been asked about which Linux distribution I use a few times so I thought that it would be nice to share it here.*

When I started using Linux, Ubuntu depended on the GNOME 2 desktop environment. It was simple but looked goood at the same time (and it was all brown/orange-ish). However, I didn't like it when they decided to drop GNOME and start using their own custom environment, Unity. Since then, I have been installing GNOME 3 on top of Unity on my Ubuntu machines.
The process became easier when a group of users decided to create the Ubuntu GNOME distribution, which includes GNOME as the default environment. I started using this distro as soon as it came out.
However, more recently I have been experimenting with other distributions like Arch and Fedora to see how difference they are from Debian and Ubuntu. Arch was very customizable and had a strong DIY philosophy, but as a consequence it was really hard to setup and fix if the system broke. On the other hand, Fedora 20 actually impressed me with it's level of polish and integration with GNOME 3, and it includes a package manager that is similar to what you would expect in a Debian system.

## Why GNOME 3?

Some people don't care too much about how their system looks as long as it gets the job done. However, one of the reasons I use Linux instead of Windows is because I don't really like the UI (outside of Metro). So the environment I choose should look good and have a consistent design.
Gnome 3 really accomplishes this. There is a strong cohesion in the overall design of every Gnome/GTK app and everything I run on the system essentially feels like part of it (except for Java apps, of course). I think it rivals Mac OSX design cohesion and leaves Windows to shame.

### Fast universal search

One of the things that Unity added to Ubuntu was universal search (kind of like Spotlight in OSX). However, while using it there was this small 1-2 second lag to open the search after you pressed the keyboard shortcut. I would end up writing stuff like "clipse" or "refox".
Windows also has a fast response time, but for some reason it spends more time loading results (an SSD might solve the problem).
Gnome shell has a really fast search, and I got used to it really fast. *Press key, type first letters of the program, enter* It may not have the same amount of lenses that Unity search has, but I think that that is what keeps it fast and lightweight.

### Window overview

Another important part of Gnome 3 for me is the overview mode that appears as soon as you press the super (or Windows) key. It really makes it easier to find windows and organize my workspaces. The feature is VERY similar to the one in Mac OSX, but I think it is nice to be able to do this sort of stuff on a non-Apple system.

## Apps

### Terminal and VIM

![Desktop screenshot](/img/posts/gnome/vim.png)

I use the Gnome Terminal with a solarized theme. Vim is my favorite editor (sorry Emac fans), but I really can't use it without plugins anymore. I use a distribution called spf13 that includes many common plugins already configured for you.

Solarized theme: <https://github.com/sigurdga/gnome-terminal-colors-solarized>

VIM spf13: <http://vim.spf13.com>

### Nautilus

Nautilus (now known just as Files) is the default file manager in Gnome. The new compact design gets rid of the menu bar and merges everything into the title bar.
One feature I really like is the server mounting feature. It lets me access SSH server in the same way I would access a USB drive or partition.

### Corebird

There are a lot of Twitter clients for Linux. Gwibber was the default on Ubuntu but it was always terribly laggy. I recently found out that a new client designed specifically for Gnome 3 was available. Corebird has a very nice design and animations, and features a realtime stream of tweets just like TweetDeck.

To install on Fedora:
```
yum install corebird
```

More info on <http://corebird.baedert.org>

### Empathy

This is my favorite chat application. The fact that it lets you write replies right on the notifications (Note: They added this before OSX did) is really useful and allows me to stay focused on the main window I'm working on and saves a few ALT-TABs
