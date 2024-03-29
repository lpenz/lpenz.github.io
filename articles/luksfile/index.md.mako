---
title: Creating an encrypted directory-in-a-file
subtitle: Using the Linux Unified Key Setup (LUKS)
date: 2020-03-22
...


${"#"} Introduction

LUKS can be used to create a file with a whole encrypted filesystem
inside, that we can mount and manipulate in-place.

This article has my notes on how to create the file and set up the
mounting system.

Obs: to ease copy-and-pasting, we show the commands without prompt, and
prepend ``> `` to the output of commands in the examples.


${"#"} File creation

We'll use **cryptsetup** to create the file, it's available in all
major distributions.

First, create a file with the desired size (64MB in this case):
```
dd if=/dev/zero of=cryptfile.img bs=1M count=64
> 64+0 records in
> 64+0 records out
> 67108864 bytes (67 MB, 64 MiB) copied, 1.36272 s, 49.2 MB/s
```

Note: be aware that ext4 doesn't support devices with less than 32MB

Use *cryptsetup luksFormat* to format it; we also choose the
passphrase in this step:
```
sudo cryptsetup luksFormat cryptfile.img
>
> WARNING!
> ========
> This will overwrite data on cryptfile.img irrevocably.
> 
> Are you sure? (Type 'yes' in capital letters): YES
> Enter passphrase for cryptfile.img:
> Verify passphrase:
```

Use *cryptsetup luksOpen* to bind the file to a block device that
can be manipulated as if it was a partition:

```
sudo cryptsetup luksOpen cryptfile.img cryptdev
> Enter passphrase for cryptfile.img:
```

After *cryptsetup luksOpen*, we are able to access the new block
device at ``/dev/mapper/cryptdev``. It's now time to create the
filesystem structure:

```
sudo mkfs.ext4 /dev/mapper/cryptdev
> mke2fs 1.45.5 (07-Jan-2020)
> Creating filesystem with 49152 1k blocks and 12288 inodes
> Filesystem UUID: c08602c2-acf7-4acf-b007-dce1200067d7
> Superblock backups stored on blocks:
>            8193, 24577, 40961
> 
> Allocating group tables: done
> Writing inode tables: done
> Creating journal (4096 blocks): done
> Writing superblocks and filesystem accounting information: done
```

We can now finally close the device
```
sudo cryptsetup luksClose cryptdev
```


${"#"} Testing by mounting

To mount the file, first get the block device again:
```
sudo cryptsetup luksOpen cryptfile.img cryptdev
> Enter passphrase for cryptfile.img:
```

And then mount as usual:
```
mkdir -p cryptdir
sudo mount -t auto /dev/mapper/cryptdev cryptdir
```

We always end up with a *lost+found* directory when we use the usual linux filesystems:
```
ls -l cryptdir/
> total 12
> drwx------ 2 root root 12288 Mar 22 17:47 lost+found
```

Don't forget to unmount and close the device after using it:
```
sudo umount cryptdir
sudo cryptsetup luksClose cryptdev
```


${"#"} Setting up user mount

**cryptmount** allows us to mount configured encrypted files without
root permissions.

Create a */etc/cryptmount/cmtab* file with the following contents:

```
mycryptlabel {
    dev=$(HOME)/cryptfile.img
    dir=$(HOME)/cryptdir
    fstype=auto
    keyformat=luks
}
```

cryptmount is now able to mount the file.


${"#"} Mounting

Before mounting, you may want to go into a private mount namespace:

```
sudo unshare -m sudo -u "$USER" -i
> [sudo] password for $USER:
```

That starts a new shell in a new private mount namespace. Everything
you mount from this shell it will be available only to processes
started from this shell, and all mounts will be automatically
unmounted when the last of these processes exit.

You can now run the following command to mount the file:

```
cryptmount mycryptlabel
> Enter password for target "mycryptlabel":
> e2fsck 1.45.5 (07-Jan-2020)
> /dev/mapper/cryptdev: clean, 33/4096 files, 1896/16384 blocks
```

Don't forget to unmount it later, and leave the shell:

```
cryptmount -u mycryptlabel
exit
```


${"#"} Final remarks

It should be said that these notes only touch the surface of
cryptsetup and cryptmount. They have a lot of options and can be used
in many different ways.

One interesting option is to keep the cryptographic key in a separate
file. That can be useful if you want to keep a rather large mountable
file in an exposed backed-up piece of infrastructure while minimizing
exposure.

All of these commands can be used to create and mount an encrypted
partition, just replace *cryptfile.img* with the path to the
partition block device. You can also create an encrypted partition
that encompasses an entire flash drive, leaving it without a partition
table at all, by using the appropriate block device.


${"#"} References

- <https://linux.die.net/man/8/cryptsetup>
- <https://linux.die.net/man/8/cryptmount>
- <https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/>

