---
title: Using Fedora 23 on a MacBook Pro
date: Mon Dec 28 09:29:00 EDT 2015
author: Eduardo Trujillo
uuid: af03d9f4-1318-43da-9852-a673db71e5dc
---

For the past few months, I've been using Fedora as my main OS on my laptop, a
2014-ish MacBook Pro. OSX is not bad, but it does not have native support for
things like Docker.

I still have my OSX partition setup in case I need something that I can't do on
Linux (like updating the firmware or enabling/disabling the startup sound).

These are some of notes I've collected along the way:

## Writing the ISO to a USB stick

If you are installing Fedora on a MBP, you are probably running OSX initially.
How do you write the ISO image into a USB stick and make it bootable?

I've found that [Ubuntu's guide][ubuntu_guide] on the topic is the easiest
one to follow and works with the Fedora image too.

Here's a quick summary:

```bash
hdiutil convert -format UDRW -o ~/path/to/target.img ~/path/to/fedora.iso
diskutil unmountDisk /dev/diskN
sudo dd if=~/path/to/target.img of=/dev/diskN bs=1m
diskutil eject /dev/diskN
```

See below on how to boot into the live image.

## Dual-booting is possible without any additional software

Some guides for installing Linux on MacBooks point you to installing UEFI
software like rEFInd. I would consider this an optional step since the laptop's
firmware has an OS selection screen built-in.

For both, booting the live image and picking which OS to boot, all you have to
do is hold down the `alt` key while the laptop is booting and pick what you
want to boot. If you formatted the USB stick correctly, Fedora should be an
option.

> **TIP:** You can setup a firmware password to protect the OS selection
screen (just like it would on a PC).

## Going online (wireless-ly)

Like on other laptops on Linux, the wireless card needs some drivers before you
can use it.

On Fedora, the MacBook card's driver is available under the `akmod-wl` package.
However, you will first need to enable the RPM fusion repositories (both free
and non-free).

The easiest way to do this is to use a Thunderbolt Ethernet adapter or to
tether your phone. Otherwise, you will porbably need to copy a couple of RPMs.

> **TIP:** On some cases, the `kernel-headers` package will have a different
version from the current kernel in your system. This will prevent akmod from
compiling the module. You should run `dnf upgrade` if that happens.

After restarting, the module should be compiled and you should be able to
connect to an access point.

## Turning off that red light

After booting Fedora (or any other Linux distribution), you will most likely
notice that there is an odd red light coming out your laptop's headphone jack.
This is normal. The headphone jack on MacBooks has a S/PDIF port built-in.

The reason why the light is on while using Linux is that, by default, Linux
does not enable power saving mode on the audio card. However, its very easy to
enable it:

```bash
# As root:
echo 1 > /sys/module/snd_hda_intel/parameters/power_save
```

Additional power saving toggles can be found using `powertop`.

## Go to sleep

For some reason, something keeps waking up the laptop after you press the
power button or close the lid. Having you laptop turn on inside your bag is not
fun!

The workaround seems to disable a couple of wakeup triggers:

```bash
# As root:
echo LID0 > /proc/acpi/wakeup
echo XHC1 > /proc/acpi/wakeup
```

The side effect being that your laptop won't turn on automatically when you
open the lid, however, you can now safely put it in your bag.

## The webcam

The only piece of hardware that you'll find that does not work on Linux is the
webcam. This is due to the fact that the drivers for it are not open source.

There is, however, an [ongoing effort][effort] to build/reverse-engineer a
driver for Linux.

## As a systemd unit

I wrote a small unit file that will fix the red light and wake up issues at
boot time:

```ini
[Unit]
Description=Improve MacBook setup

[Service]
Type=oneshot
ExecStart=/bin/bash -c "echo 1 > /sys/module/snd_hda_intel/parameters/power_save && echo LID0 > /proc/acpi/wakeup && echo XHC1 > /proc/acpi/wakeup"

[Install]
WantedBy=multi-user.target
```

Save this file as `/etc/systemd/system/mbp.service` and run 
`systemctl daemon-reload && systemctl enable mbp` to load the unit on every
boot.

[ubuntu_guide]: http://www.ubuntu.com/download/desktop/create-a-usb-stick-on-mac-osx
[effort]: https://github.com/patjak/bcwc_pcie
