---
title: Creating a pmount-compatible encrypted USB drive
subtitle: Using the Linux Unified Key Setup (LUKS)
date: 2021-06-05
...

# Why pmount

There are many ways to set up how removable media is mounted (or even
auto-mounted) in linux. Out of those, *pmount* provides the following
advantages:

- users can mount drives
- out-of-the-box LUKS (encrypted) support
- no configuration, even for encrypted partitions


These are my short notes on how to format a USB drive in a way that is
compatible with pmount.


# Identifying the USB drive

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


# Creating the luks-encrypted partition

All examples assume that an empty USB drive was inserted and identified as
`/dev/sdb`. The commands below are dangerous, do not use them if you don't
understand what they are doing.


## Partition the drive

We can use `fdisk` or any other partitioning tool. An example session:

```
$ {b}sudo fdisk /dev/sdb{/b}
Welcome to fdisk (util-linux 2.36.1).
Changes will remain in memory only, until you decide to write them.
Be careful before using the write command.
Device does not contain a recognized partition table.
Created a new DOS disklabel with disk identifier 0x0677b040.
Command (m for help): {b}n{/b}
Partition type
   p   primary (0 primary, 0 extended, 4 free)
   e   extended (container for logical partitions)
Select (default p): {b}p{/b}
Partition number (1-4, default 1): {b}{/b}
First sector (2048-102399, default 2048): {b}{/b}
Last sector, +/-sectors or +/-size{K,M,G,T,P} (2048-102399, default 102399): {b}{/b}
Created a new partition 1 of type 'Linux' and of size 49 MiB.
Command (m for help): {b}w{/b}
The partition table has been altered.
Syncing disks.
```

The partition type doesn't matter much, it has no relation with the file system
in linux. Use `83` (Linux) if in doubt.


## Format the partition

Use *cryptsetup luksFormat* to format the partition; we also choose the
passphrase in this step. Let's create a variable with the partition
(`/dev/sdb1` in this example) and format it:

```
$ {b}part=/dev/sdb1{/b}
$ {b}sudo cryptsetup luksFormat --label cryptflash "$part"{/b}
WARNING: Device /dev/sdb1 already contains a 'dos' partition signature.
WARNING!
========
This will overwrite data on /dev/sdb1 irrevocably.
Are you sure? (Type 'yes' in capital letters): {b}YES{/b}
Enter passphrase for /dev/sdb1: {b}mypassphrase{/b}

Verify passphrase: {b}mypassphrase{/b}
```


## Create the filesystem

This is a two-step process: we first "open" the luks partition, creating a
pseudo-device; and then we create the filesystem we want on top of that. The
filesystem layer doesn't care if the device is an abstract cryptographic engine
or a real partition.

Let's open the luks partition in the pseudo-device `cryptdev`:

```
$ {b}sudo cryptsetup luksOpen "$part" cryptdev{/b}
Enter passphrase for /dev/sdb1: {b}mypassphrase{/b}
```

We now have to create the filesystem. The
[pmount manual](https://linux.die.net/man/1/pmount) has a list of supported
filesystems. *ext4* is the most popular one for non-removable drives, but it's
not great for drives that we want to use in multiple systems, as the UIDs are
hard-coded in the FS. That means that we don't have the proper permissions when
we plug the drive in other systems where our UID has a different numerical
value. We'd then have to use *sudo*, which nullifies one of the advantages of
*pmount*.

For this reason, we are using
[UDF](https://en.wikipedia.org/wiki/Universal_Disk_Format) for the partition:

```
$ {b}sudo mkfs.udf -l cryptflash /dev/mapper/cryptdev{/b}
filename=/dev/mapper/cryptdev
label=cryptflash
uuid=61d338188d37bb6e
blocksize=512
blocks=69632
udfrev=2.01
start=0, blocks=64, type=ERASE
start=64, blocks=13, type=VRS
start=77, blocks=19, type=ERASE
start=96, blocks=16, type=MVDS
start=112, blocks=16, type=ERASE
start=128, blocks=16, type=LVID
start=144, blocks=112, type=ERASE
start=256, blocks=1, type=ANCHOR
start=257, blocks=69112, type=PSPACE
start=69369, blocks=6, type=ERASE
start=69375, blocks=1, type=ANCHOR
start=69376, blocks=96, type=ERASE
start=69472, blocks=16, type=RVDS
start=69488, blocks=143, type=ERASE
start=69631, blocks=1, type=ANCHOR
mkudffs: Warning: Creating new UDF filesystem on partition, and not on whole disk device
mkudffs: Warning: UDF filesystem on partition cannot be read on Apple systems
```

At last, we close the luks layer with:

```
$ {b}sudo cryptsetup luksClose cryptdev{/b}
```


# Mounting/unmounting the drive

We could *luksOpen* the device and just use the regular *mount* to access the
files - but using pmount is much more convenient, as it takes care of doing both
steps and doesn't require any configuration or permissions.

All partitions in block devices pop up in `/dev/disk/by-label/` as symbolic
links to the underlying `/dev` entry. We can simply use that path as the
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


# References

- <https://linux.die.net/man/8/cryptsetup>
- <https://linux.die.net/man/8/cryptmount>
- <https://linux.die.net/man/1/pmount>
- <https://linux.die.net/man/1/pumount>
- <https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/>
- <https://en.wikipedia.org/wiki/Universal_Disk_Format>
- [Different UID/GID when using an ext4 formatted USB drive with another computer](https://unix.stackexchange.com/questions/38309/different-uid-gid-when-using-an-ext4-formatted-usb-drive-with-another-computer):
  why using ext4 on a USB drive is not a great idea
