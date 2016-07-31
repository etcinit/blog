---
title: Automatically TRIM-ing your SSD using systemd
date: Sun Jul 30 17:34:50 UTC 2016
author: Eduardo Trujillo
uuid: ac1f6bf6-72be-4729-a545-6d6b08112149
---

If you have a Linux laptop or desktop with a solid-state drive, and happen to
have disk encryption enabled through a LUKS/LVM combo, getting [TRIM][1]
support enabled isn't a very straightforward process. It has to be enabled on
every IO layer.

In their blog post, [_How to properly activate TRIM for your SSD on Linux:
fstrim, lvm and dm-crypt_][2], Carlos Lopez gives a brief introduction on what
TRIM is, and explains why it is beneficial to enable it. The article also
describes the steps needed to enable this functionality on each IO layer
(dm-crypt, LVM, and the filesystem).

I followed most of this guide for one my own systems, and while I followed
their advice and avoided enabling the discard flag on the filesystem, I never
set up a cron job for running the trim operation periodically. So I found
myself manually executing [`fstrim`][3] every now and then by hand.

This quickly became slightly repetitive, so I began looking into setting up the
automation part. The guide above had an example setup using cron. However, I
never set up a cron daemon on my system. So I wondered if it was possible to
achieve the same result using systemd.

After reading some documentation on systemd unit files, I learned that is
possible to setup timers for your service units, which effectively achieves the
same result as a cron daemon.

Below I'm including a fstrim service and timer. The service mainly specifies
which command to run and the timer defines how often it should be executed.
Note that the service unit does not have a `WantedBy` option and its `Type` is
`oneshot`. This means it won't be automatically executed, and that it is
intended to be a one off command, not a daemon. The timer does have a
`WantedBy` option, which will result on it being started at boot.

I can check the status of the timer by using `systemctl list-times` and also
run the operation on demands by starting the service unit: `systemctl start
fstrim`. The logs are stored on the journal, which can be queried with
`journalctl -u fstrim`.

##### /etc/systemd/system/fstrim.service

This is the service file. Here you can customize how `fstrim` is invoked. I use
the `a` and `v` options, which tell `fstrim` to automatically run on every
drive and print verbose output. Additionally, this assumes `fstrim` is
installed at `/sbin/fstrim`.

``` {#fstrimservice .ini}
[Unit]
Description=Run fstrim on all drives

[Service]
Type=oneshot
ExecStart=/sbin/fstrim -av
User=root
```

##### /etc/systemd/system/fstrim.timer

In this configuration, the `fstrim` command is executed by `root` 15 minutes
after booting the machine and weekly afterwards.

``` {#fstrimtimer .ini}
[Unit] Description=Run fstrim on boot and weekly afterwards

[Timer]
OnBootSec=15min
OnUnitActiveSec=1w 

[Install]
WantedBy=timers.target
```

[1]: https://en.wikipedia.org/wiki/Trim_(computing)
[2]: http://blog.neutrino.es/2013/howto-properly-activate-trim-for-your-ssd-on-linux-fstrim-lvm-and-dmcrypt/
[3]: https://packages.gentoo.org/packages/sys-apps/util-linux
