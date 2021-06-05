---
title: Creating a pmount-compatible encrypted USB drive
subtitle: Using the Linux Unified Key Setup (LUKS)
date: 2021-06-05
...

${"#"} Why pmount

There are many ways to set up the way removable media is mounted (or even
auto-mounted) in linux. Out of those, *pmount* provides the following
advantages:

- users can mount drives
- out-of-the-box LUKS (encrypted) support
- no configuration, even for encrypted partitions


These are my short notes on how to format a USB drive in a way that is
compatible with pmount.


${"#"} Identifying the USB drive

`lsblk -f` shows all block devices in the system. If we run it before inserting
the drive, and then after, the inserted drive is the new device in the output:

```
$ lsblk -f
NAME   FSTYPE      FSVER LABEL       UUID                                 FSAVAIL FSUSE% MOUNTPOINT
sda
|-sda1
|-sda2 ext4        1.0   sddroot     38e24d86-4374-4042-aebe-eeaec2317b6f    5.5G    84% /
sdb
```

We can also use
[watch (1)](https://linux.die.net/man/1/watch) or
[ogle](https://github.com/lpenz/ogle/) (shameless plug) to monitor the output
of `lsblk` and get an immediate feedback when the drive is detected.


${"#"} Creating the luks-encrypted partition

All examples assume that an empty USB drive was inserted and identified as
`/dev/sdb`. Do not use the commands below if you don't understand what they are
doing. If you have a second hard-drive identified as `/dev/sdb`, for instance,
the commands below will make it unusable.


${"##"} Partition the drive

We can use `fdisk` or any other partitioning tool. An example session:

```
${fdisk}
```

The partition type doesn't matter much, it has no relation with the file system
in linux. Use `83` (Linux) if in doubt.


${"##"} Format the partition

Use *cryptsetup luksFormat* to format the partition; we also choose the
passphrase in this step. Let's create a variable with the partition
(`/dev/sdb1` in this example) and format it:

```
${luksformat}
```


${"##"} Create the filesystem

This is a two-step process: we first "open" the luks partition, creating a
pseudo-device; and then we create the filesystem we want on top of that. The
filesystem layer doesn't care if the device is an abstract cryptographic engine
or a real partition.

Let's open the luks partition in the pseudo-device `cryptdev`:

```
${luksopen}
```

We now have to create the filesystem. The
[pmount manual](https://linux.die.net/man/1/pmount) has a list of supported
filesystems. *ext4* is the most popular one for non-removable drives, but it's
not great for drives that we want to use in multiple systems, as it hard-codes
the UIDs. That means that if we won't have the proper permissions if we plug
the drive in another system where our UID has a different numerical value. We'd
then have to use *sudo*, which nullifies one of the advantages of *pmount*.

For this reason, we are using
[UDF](https://en.wikipedia.org/wiki/Universal_Disk_Format) for the partition:

```
${mkfs}
```

At last, we close the luks layer with:

```
${luksclose}
```


${"#"} Mounting/unmounting the drive

We can *luksOpen* the device and just use the regular *mount* to access the
files. But using pmount is much more convenient, as it takes care of doing both
steps and doesn't require any configuration or permissions.

All partitions in block devices pop up in `/dev/disk/by-label/` as a symbolic
link to the underlying `/dev` entry. We can simply use that path as the
argument to pmount to get the corresponding filesystem mounted at `/media/`:

```
$ {b}pmount /dev/disk/by-label/cryptflash{/b}
```

To unmount:

```
$ {b}pumount /media/disk_by-label_cryptflash/{/b}
```

We can also create a simple script that does that when given the label:

```
#!/bin/bash

label=#dollar#{1?Usage $0 <label>}

set -e -x

pmount "/dev/disk/by-label/$label"
: mounted at "/media/disk_by-label_#dollar#{label}"
```


${"#"} References

- <https://linux.die.net/man/8/cryptsetup>
- <https://linux.die.net/man/8/cryptmount>
- <https://linux.die.net/man/1/pmount>
- <https://linux.die.net/man/1/pumount>
- <https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/>
- <https://en.wikipedia.org/wiki/Universal_Disk_Format>
- [Different UID/GID when using an ext4 formatted USB drive with another computer](https://unix.stackexchange.com/questions/38309/different-uid-gid-when-using-an-ext4-formatted-usb-drive-with-another-computer):
  why using ext4 on a USB drive is not a great idea
