<div id="toc" style="margin-bottom: 2em;">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#partitioning" id="toc-partitioning"><span
class="toc-section-number">1</span> Partitioning</a></li>
<li><a href="#installing-the-bootloader"
id="toc-installing-the-bootloader"><span
class="toc-section-number">2</span> Installing the bootloader</a></li>
<li><a href="#installing-the-base-system-in-the-root-partition"
id="toc-installing-the-base-system-in-the-root-partition"><span
class="toc-section-number">3</span> Installing the base system in the
root partition</a></li>
<li><a href="#on-root-configuration"
id="toc-on-root-configuration"><span class="toc-section-number">4</span>
On-root configuration</a></li>
<li><a href="#closing-up" id="toc-closing-up"><span
class="toc-section-number">5</span> Closing up</a></li>
<li><a href="#references" id="toc-references"><span
class="toc-section-number">6</span> References</a></li>
</ul>
</div>
<p><strong>Updated 2019-06-25</strong>: use Debian Stretch explicitly
(already implied by kernel version); suggest testing with qemu.</p>
<p>We can install a full Linux distribution in a USB drive, and use it
to boot a system. That is called a “live USB,” and it can be used for
recovery, as a portable environment, etc.</p>
<p>In this article we explain how to install a Debian GNU/Linux OS in a
USB drive as if it was a hard disk. We will use Debian’s own
<em>debootstrap</em> to populate the root partition and
<em>syslinux</em> as a bootloader (it is simpler than the standard
<em>grub</em>).</p>
<p>Obs: to ease copy-and-pasting, we show the commands without prompt,
and prepend <code>&gt;</code> to the output of commands on most
examples.</p>
<h1 data-number="1" id="partitioning"><span
class="header-section-number">1</span> Partitioning</h1>
<p>After inserting the USB drive, it will appear as a block device under
<em>/dev</em>, usually <em>sd[a-z]</em>. Take a note on the device name.
We will use <em>/dev/sdc</em> through the examples.</p>
<p>Create two partitions on our USB drive: one with 256MB for the
<em>/boot</em> that will hold the <em>syslinux</em> bootloader and the
Linux kernel; and another with all the rest of the space, that will hold
the root filesystem.</p>
<p>There are many utilities you can use to partition the USB drive:
<em>parted</em>, <em>fdisk</em>, etc.</p>
<p>For example, using fdisk, run it on <em>/dev/sdc</em>:</p>
<pre><code>fdisk /dev/sdc
&gt; Welcome to fdisk (util-linux 2.25.2).
&gt; Changes will remain in memory only, until you decide to write them.
&gt; Be careful before using the write command.</code></pre>
<p>You are left at <em>fdisk</em>’s command prompt. Create the boot
partition:</p>
<pre><code>Command (m for help): n
&gt; Partition type
&gt;    p   primary (0 primary, 0 extended, 4 free)
&gt;    e   extended (container for logical partitions)
&gt; Select (default p): p
&gt; Partition number (1-4, default 1): 1
&gt; First sector (2048-31350782, default 2048):
&gt; Last sector, +sectors or +size{K,M,G,T,P} (2048-31350782, default 31350782): +256M
&gt; 
&gt; Created a new partition 1 of type &#39;Linux&#39; and of size 256 MiB.</code></pre>
<p>Set its type to <em>FAT16</em>:</p>
<pre><code>Command (m for help): t
&gt; Selected partition 1
&gt; Hex code (type L to list all codes): 6
&gt; If you have created or modified any DOS 6.x partitions, please see the fdisk documentation for additional information.
&gt; Changed type of partition &#39;Linux&#39; to &#39;FAT16&#39;.</code></pre>
<p>Mark it as <em>active</em>:</p>
<pre><code>Command (m for help): a
&gt; The bootable flag on partition 1 is enabled now.</code></pre>
<p>Create the root partition:</p>
<pre><code>Command (m for help): n
&gt; Partition type
&gt;    p   primary (1 primary, 0 extended, 3 free)
&gt;    e   extended (container for logical partitions)
&gt; Select (default p): p
&gt; Partition number (2-4, default 2): 2
&gt; First sector (526336-31350782, default 526336):
&gt; Last sector, +sectors or +size{K,M,G,T,P} (526336-31350782, default 31350782):
&gt; 
&gt; Created a new partition 2 of type &#39;Linux&#39; and of size 14.7 GiB.</code></pre>
<p>Check that they were created:</p>
<pre><code>Command (m for help): p
&gt; Disk /dev/sdc: 15 GiB, 16051600896 bytes, 31350783 sectors
&gt; Units: sectors of 1 * 512 = 512 bytes
&gt; Sector size (logical/physical): 512 bytes / 512 bytes
&gt; I/O size (minimum/optimal): 512 bytes / 512 bytes
&gt; Disklabel type: dos
&gt; Disk identifier: 0x13090bb3
&gt; 
&gt; Device     Boot  Start      End  Sectors  Size Id Type
&gt; /dev/sdc1  *      2048   526335   524288  256M  6 FAT16
&gt; /dev/sdc2       526336 31350782 30824447 14.7G 83 Linux</code></pre>
<p>Save and exit:</p>
<pre><code>Command (m for help): w
&gt; The partition table has been altered.
&gt; Calling ioctl() to re-read partition table.
&gt; Syncing disks.</code></pre>
<p>We now have a <em>/dev/sdc1</em> that will be our <em>/boot</em>, and
<em>/dev/sdc2</em> that will be our root file system. Observe that the
boot partition has a MS-DOS type - that is required
<em>syslinux</em>.</p>
<p>(the instructions above are heavily based on <a
href="http://allskyee.blogspot.com.br/2014/01/using-syslinux-to-boot-debootstraped.html">using-syslinux-to-boot-debootstraped</a>)</p>
<h1 data-number="2" id="installing-the-bootloader"><span
class="header-section-number">2</span> Installing the bootloader</h1>
<p>Create a FAT16 filesystem on the boot device:</p>
<pre><code>mkdosfs -n LINUXBOOT /dev/sdc1
&gt; mkfs.fat 3.0.27 (2014-11-12)</code></pre>
<p>Install <em>syslinux</em> on it:</p>
<pre><code>syslinux /dev/sdc1</code></pre>
<h1 data-number="3"
id="installing-the-base-system-in-the-root-partition"><span
class="header-section-number">3</span> Installing the base system in the
root partition</h1>
<p>Create the filesystem:</p>
<pre><code>mkfs.ext4 /dev/sdc2
&gt; mke2fs 1.42.12 (29-Aug-2014)
&gt; Creating filesystem with 3853055 4k blocks and 964768 inodes
&gt; Filesystem UUID: 68d66fd5-97f2-46ed-aee6-dad6f228a172
&gt; Superblock backups stored on blocks:
&gt;         32768, 98304, 163840, 229376, 294912, 819200, 884736, 1605632, 2654208
&gt; 
&gt; Allocating group tables: done
&gt; Writing inode tables: done
&gt; Creating journal (32768 blocks): done
&gt; Writing superblocks and filesystem accounting information: done</code></pre>
<p>You can use any filesystem here, as long as it is supported by your
future kernel.</p>
<p>Mount both partitions and use <em>debootstrap</em> to install the
base files on it:</p>
<pre><code>mkdir -p usbroot
mount -t auto /dev/sdc2 usbroot
mkdir -p usbroot/boot
mount -t auto /dev/sdc1 usbroot/boot
debootstrap stretch usbroot http://ftp.debian.org/debian
&gt; I: Retrieving Release.gpg 
&gt; I: Checking Release signature
&gt; I: Valid Release signature (key id 75DDC3C4A499F1A18CB5F3C8CBF8D6FD518E17E1)
&gt; I: Retrieving Packages 
&gt; I: Validating Packages 
&gt; I: Resolving dependencies of required packages...
&gt; I: Resolving dependencies of base packages...
&gt; I: Found additional required dependencies: acl adduser dmsetup insserv libaudit1 libaudit-common libbz2-1.0 libcap2 libcap2-bin libcryptsetup4 libdb5.3 libdebconfclient0 libdevmapper1.02.1 libgcrypt20 libgpg-error0 libkmod2 libncursesw5 libprocps3 libsemanage1 libsemanage-common libslang2 libsystemd0 libudev1 libustr-1.0-1 procps systemd systemd-sysv udev 
&gt; I: Found additional base dependencies: libdns-export100 libffi6 libgmp10 libgnutls-deb0-28 libgnutls-openssl27 libhogweed2 libicu52 libidn11 libirs-export91 libisccfg-export90 libisc-export95 libmnl0 libnetfilter-acct1 libnettle4 libnfnetlink0 libp11-kit0 libpsl0 libtasn1-6 
&gt; I: Checking component main on http://ftp.debian.org/debian...
&gt; I: Retrieving acl 2.2.52-2
&gt; (...)
&gt; I: Configuring libc-bin...
&gt; I: Configuring systemd...
&gt; I: Base system installed successfully.</code></pre>
<h1 data-number="4" id="on-root-configuration"><span
class="header-section-number">4</span> On-root configuration</h1>
<p>We will have to <em>chroot</em> into our root filesystem to configure
it further.</p>
<p>Mount the boot device and the default ones inside the root mount
point:</p>
<pre><code>mount -t devtmpfs dev       usbroot/dev
mount -t devpts   devpts    usbroot/dev/pts
mount -t proc     proc      usbroot/proc
mount -t sysfs    sysfs     usbroot/sys</code></pre>
<p><em>chroot</em> into root:</p>
<pre><code>chroot usbroot /bin/bash</code></pre>
<p>Set the root user password:</p>
<pre><code>passwd
&gt; Enter new UNIX password:
&gt; Retype new UNIX password:
&gt; passwd: password updated successfully</code></pre>
<p>Install the Linux kernel and other important packages:</p>
<pre><code>apt-get install --no-install-recommends -y linux-image-amd64 syslinux busybox-static
&gt; Reading package lists... Done
&gt; Building dependency tree... Done
&gt; The following extra packages will be installed:
&gt;   initramfs-tools klibc-utils libklibc libuuid-perl linux-base linux-image-4.9.0-9-amd64
&gt; Suggested packages:
&gt;   bash-completion linux-doc-3.16 debian-kernel-handbook grub-pc grub-efi extlinux
&gt; Recommended packages:
&gt;   busybox busybox-initramfs busybox-static firmware-linux-free irqbalance
&gt; The following NEW packages will be installed:
&gt;   initramfs-tools klibc-utils libklibc libuuid-perl linux-base linux-image-4.9.0-9-amd64 linux-image-amd64
&gt; 0 upgraded, 7 newly installed, 0 to remove and 0 not upgraded.
&gt; Need to get 34.1 MB of archives.
&gt; After this operation, 164 MB of additional disk space will be used.
&gt; (...)
&gt; Setting up linux-image-amd64 (3.16+63) ...
&gt; Processing triggers for initramfs-tools (0.120) ...
&gt; ln: failed to create hard link &#39;/boot/initrd.img-4.9.0-9-amd64.dpkg-bak&#39; =&gt; &#39;/boot/initrd.img-4.9.0-9-amd64&#39;: Operation not permitted
&gt; update-initramfs: Generating /boot/initrd.img-4.9.0-9-amd64</code></pre>
<p>We now have to set up our mount points in the <em>/etc/fstab</em> of
the USB drive, but if we simply use <em>/dev/sdc*</em> as the devices,
we will have trouble mounting it on other systems with a different
number of hard drives. To have stable mount points, we use the
<em>UUID</em> - universal unique identifiers - of the filesystems. Use
<em>blkid</em> to find out the values of your identifiers:</p>
<pre><code>blkid
&gt; (...)
&gt; /dev/sdc1: SEC_TYPE=&quot;msdos&quot; UUID=&quot;2420-26B1&quot; TYPE=&quot;vfat&quot; PARTUUID=&quot;13090bb3-01&quot;
&gt; /dev/sdc2: UUID=&quot;68d66fd5-97f2-46ed-aee6-dad6f228a172&quot; TYPE=&quot;ext4&quot; PARTUUID=&quot;13090bb3-02&quot;</code></pre>
<p>In this example, the <em>UUID</em> of the boot filesystem is
<code>2420-26B1</code>, and the <em>UUID</em> of the root filesystem is
<code>68d66fd5-97f2-46ed-aee6-dad6f228a172</code>. Use them to populate
<em>/etc/fstab</em>:</p>
<pre><code>echo &#39;UUID=68d66fd5-97f2-46ed-aee6-dad6f228a172 /     ext4 defaults,noatime 0 0&#39; &gt;  etc/fstab
echo &#39;UUID=2420-26B1                            /boot vfat defaults         0 0&#39; &gt;&gt; etc/fstab</code></pre>
<p>Figure out the name of the kernel and initrd installed on the boot
partition:</p>
<pre><code>ls boot/vmlinuz* boot/initrd*
&gt; boot/initrd.img-4.9.0-9-amd64  boot/vmlinuz-4.9.0-9-amd64</code></pre>
<p>And use them with the <em>UUID</em>s to create the
<code>boot/syslinux.cfg</code> file, with the following contents:</p>
<pre><code>default linux
timeout 1
prompt 1

label linux
    kernel vmlinuz-4.9.0-9-amd64
    append initrd=initrd.img-4.9.0-9-amd64 root=UUID=68d66fd5-97f2-46ed-aee6-dad6f228a172 ro</code></pre>
<p>Finally, write <em>syslinux</em>’s master boot record on the USB
drive:</p>
<pre><code>cat /usr/lib/SYSLINUX/mbr.bin &gt; /dev/sdc</code></pre>
<h1 data-number="5" id="closing-up"><span
class="header-section-number">5</span> Closing up</h1>
<p>We are now ready to leave the <em>chroot</em> and umount all
devices:</p>
<pre><code>exit
umount usbroot/dev/pts
umount usbroot/dev
umount usbroot/proc
umount usbroot/sys
umount usbroot/boot
umount usbroot</code></pre>
<p>We can test our system using qemu:</p>
<pre><code>qemu-system-x86_64 -m 512 -hda /dev/sdc</code></pre>
<p>We should be able to login as <em>root</em>, with the password we set
above.</p>
<p>If everything is working as expected, we can now remove the USB drive
and use it to boot any computer.</p>
<h1 data-number="6" id="references"><span
class="header-section-number">6</span> References</h1>
<p>This article is, in fact, basically a rehash of the following
references with the <em>UUID</em> part added.</p>
<ul>
<li><a
href="http://allskyee.blogspot.com.br/2014/01/using-syslinux-to-boot-debootstraped.html"
class="uri">http://allskyee.blogspot.com.br/2014/01/using-syslinux-to-boot-debootstraped.html</a></li>
<li><a
href="http://quietsche-entchen.de/cgi-bin/wiki.cgi/ariane/BootableUsbStick"
class="uri">http://quietsche-entchen.de/cgi-bin/wiki.cgi/ariane/BootableUsbStick</a></li>
</ul>
<p>I’ve also created a <a
href="https://gist.github.com/lpenz/e7339a0b309e29698186baee92370104">gist</a>
that’s easy to change, with the commands in this article.</p>
