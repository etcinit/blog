---
title: First Impressions on Using Gentoo
date: Sun Mar 13 16:01:00 EDT 2016
author: Eduardo Trujillo
uuid: c6dbbd2f-4e46-469f-82c6-a3a259fe7ca7
---

![screenfetch output inside GNOME + Wayland](/images/posts/gentoo-screenfetch.png)

Not too long ago I left OS X and installed Fedora on my laptop. Now, over the
past few days, I've been working on bootstrapping a Gentoo install from within
my Fedora setup.

Unlike other Linux distributions, [Gentoo](https://gentoo.org) does not have an
installer. It is expected that you setup and install the system yourself. Yes.
It is tedious and definitely takes longer than following a setup wizard, but on
the other hand you gain some knowledge on how Linux works and end up with a
custom system.

The learning factor was what convinced me to give Gentoo a try.

Given the particular route I decided to take, the
[Gentoo Handbook](https://wiki.gentoo.org/wiki/Handbook:Main_Page) only helped
on certain parts of the process. On a normal setup, you start with a live
image of Gentoo containing just enough tools to install the system. It is also
assumed that you will start with an empty disk or one that you don't mind
erasing.

On my case, I have an SSD that has a Fedora and OS X partitions, which are both
encrypted using each operating system's built-in encryption methods. The Linux
side consisted of a
[LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) partition
containing a
[LVM](https://en.wikipedia.org/wiki/Logical_Volume_Manager_%28Linux%29) setup.

Thus, the first step was to figure out how to make space for Gentoo while also
avoid breaking the existing systems. Having LVM setup, certainly helped here.
I shrunk my Fedora root (`/`) while keeping my home partition (`/home`) intact.

![Diagram of my partitioning setup](/images/posts/gentoo-partitions.svg)

After that was done, I created a new partition for the root Gentoo filesystem
and followed the section of the handbook on how to download the _Stage3_
archive and chrooting into the environment.

Once, chroot-ed, you can sort of begin using Gentoo and install packages using
Portage. You can even run X programs if you have an X server running on the
host. However, the goal here was to also boot into Gentoo directly. A chroot-ed
environment meant that the system is still running using the Fedora kernel.
A big part of installing Gentoo is building your own kernel and then rebooting
into it.

Building a bootable kernel wasn't too hard. I installed the kernel sources
using Portage, configured it using `make menuconfig`, and compiled it. Once it
was finished. I copied over to my `/boot` partition so that GRUB could find it.

A few test boots showed that the system was booting correctly (and very fast
too). Unfortunately, once booted nothing happened because the disk partition
is encrypted using LUKS.

Getting LUKS and LVM to work was bit painful due to how long it took me to find
a working solution.

My initial attempt resulted in me trying to setup an _initramfs_ using Fedora's
`dracut` tool, which was available on the Gentoo repository and mentioned in
many guides on the distribution's wiki. It seemed logical given that it was the
same tool being used by the Fedora install, so getting it to work with Gentoo
should have only consisted of building a similar image but pointing to the
Gentoo partition.

That did not work out well. I was simply not able to get past the disk
unlocking and mounting process. I think it may have been just trying to make
many things work together: EFI, `systemd`, `plymouth`, `cryptsetup`, and `lvm`.

So, I continued with the other option presented by the Gentoo wiki,
`genkernel`. This is a tool used to automate the process of building a kernel
in Gentoo, but it also supports automatically building an initramfs. I
initially tried to avoid it given that I wanted to build the kernel myself as a
learning exercise. However, after installing it, I was pleased to find that
building an initramfs did not involved building a kernel with `genkernel`.

A few key things to enable on my setup was the LUKS and LVM options on
`/etc/genkernel.conf`, and adding `dolvm` to my kernel command line on GRUB.

_BAM!_ It booted, asked for my password, and dropped my into a login shell.

I still had to go back to Fedora and `chroot` a couple of more times until I
got the right network drivers compiled. It was nice to find out that plugging
in a Thunderbolt Ethernet adapter worked and did not crashed the kernel like it
does on Fedora.

Another thing I didn't realize immediately was that the `genkernel` initramfs
does not use `systemd` by default, so I had to add that to the kernel command
line too.

Then I continued to install software and drivers, but this time from within
Gentoo. GNOME took a good 4-6 hours to compile, mainly due to WebKit taking so
long.

Once I started GDM, NetworkManager, and the bluetooth service using
`systemctl`, I finally felt like I had a fully working system on my hands.

Everything worked, except one major peripheral, the trackpad: Apparently, the
`usbhid` driver was claiming the device before the right driver, so the
keyboard worked, but the trackpad was dead.

After a few hours of debugging, I gave up and decided to try compiling the
latest kernel available (_4.4.4 vs 4.1.15_), and after rebooting the trackpad
began working, including multitouch support!

Lastly, I switched from X to Wayland, given that X had an odd issue with some
letters not rendering after the laptop came back from sleep. Adding Wayland
support simply consisted of adding `wayland wayland-compositor xwayland egl` to
my `USE` flags, and then recompiled the affected packages. After another
reboot, "GNOME on Wayland" appeared as an option on the GDM login screen.

## Summary

In general, I think I'm happy with my current build and will attempt to use it
as my main driver for the next few weeks. If it turns out to not have any major
issues, I'll remove my fedora partition to clear up some space.

### The good

- **Worked, almost, out-of-the-box**: I was surprised by the amount of hardware
that simply just worked out of the box by enabling the right kernel modules:
GPU, Keyboard, Sound, Lid, Keyboard Backlight, Mic, Media Keys, USB,
Thunderbolt.
- **Needed some extra work:** Wi-Fi, Trackpad.
- **Closing the laptop actually puts it to sleep.** No workarounds needed like
in Fedora. Wayland is slightly better at this since it doesn't turn on the
screen for a few seconds while closed like Xorg does.
- Power consumptions seems to be slightly better than Fedora, especially after
enabling all the toggles on `powertop`.
- The system boots really fast. Around, maybe, 20-40 seconds from GRUB to GDM?
- The Gentoo repositories are generally closer to upstream than other
distributions, and I'm also growing to like Portage's `USE` keyword feature.
- Learned a lot about how modern Linux systems are setup.

### The bad

- It's not for everyone. Many users probably just want an OS that works out of
the box.
- The compile time of some packages can be very long (I'm looking at you 
WebKit).
- Many commercial software packages are only distributed as `deb`s or `rpm`s.
