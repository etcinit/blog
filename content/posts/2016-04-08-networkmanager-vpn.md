---
title: Automatically connecting to VPNs using NetworkManager
date: Fri April 08 16:01:00 EDT 2016
author: Eduardo Trujillo
uuid: 2bdc6147-c228-47f2-8b5f-8f71915a5ea4
feature-image: /images/posts/networkmanager-vpn.png
---

Coming from OS X, I've grown accustomed to [Tunnelblick][tb], which is one of
the best OpenVPN clients for the platform. It is not perfect and there are many
commercial offerings out there that have a much nicer user interface, however
Tunnelblick gets the job done and it's open source.

On Linux, the story is a bit different. Most distributions come with
[NetworkManager][nm], which is, as the name implies, a daemon for managing
network connections. For most systems, it is the component that takes care of
connecting to Wi-Fi networks, setting up an Ethernet connection when you plug
in the cable, and even 3G/4G modems.

NetworkManager has support for plugins, which has led it to support many VPN
protocols, [including OpenVPN][nmo]!

<div class="callout-quote">
...it was pleasant to find that it not only is integrated with the main
networking daemon, but it also supported on the UI-side...
</div>

When trying to figure out how to setup an OpenVPN client on Linux, it was
pleasant to find that it not only is integrated with the main networking
daemon, but it also supported on the UI-side, where most settings can be
tweaked.

However, Tunnelblick still had something I couldn't find how to do using
NetworkManager alone: Connecting to the VPN automatically and reconnecting on
cases where the connection is dropped.

For me, this is a _must have_ feature for VPN clients, given that I tend to
roam a lot with my laptop and can't remember to connect every time it connects
to a new network.

Some initial digging led me to an [Arch Linux wiki page][alp] describing how to
write a script which sort-of achieves what I'm looking for. However, the
approach seemed brittle and insecure, due to the fact that you have to make the
connection available to other users on the system, and in some cases write
connection secrets in plaintext.

After a while, I attempted to start writing a small daemon that would monitor
D-Bus and respond to NetworkManager events by determining if a VPN connection
should be started or stopped. An initial version was capable of determining if
the VPN connection is active. However, due to lack of free time to work on it
and the complexity of keeping track of the state of the machine, I decided to
put it on hold.

While working on this project, I did discover that NetworkManager does have
some of this functionality built-in. It turns out you can specify a VPN to
connect to as a requirement for some connections to succeed:

![Automatic VPN connection settings](/images/posts/networkmanager-vpn.png)

On Gentoo, this configuration can be accessed using `nm-connection-editor`,
which can be installed using the `gnome-extra/nm-applet` package.

This is working great so far, but it does required some manual configuration
for every new connection you setup, which can be annoying if you roam through
many Wi-Fi networks.

In the future, I might resume work on the D-Bus approach in order to automate
this a bit more. I would love it if my laptop simply did not trust any network
and automatically attempted to connect to a VPN. It would also be nice if this
is only attempted after a hotspot login is shown. For now, however, this should
be enough.

[tb]: https://github.com/Tunnelblick/Tunnelblick
[nm]: https://wiki.gnome.org/Projects/NetworkManager/
[nmo]: https://packages.gentoo.org/packages/net-misc/networkmanager-openvpn
[alp]: https://wiki.archlinux.org/index.php/NetworkManager#Use_dispatcher_to_connect_to_a_VPN_after_a_network_connection_is_established
