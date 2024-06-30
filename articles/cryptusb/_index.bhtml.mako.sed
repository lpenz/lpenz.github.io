<div id="toc">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#why-pmount"><span class="toc-section-number">1</span> Why
pmount</a></li>
<li><a href="#identifying-the-usb-drive"><span
class="toc-section-number">2</span> Identifying the USB drive</a></li>
<li><a href="#creating-the-luks-encrypted-partition"><span
class="toc-section-number">3</span> Creating the luks-encrypted
partition</a>
<ul>
<li><a href="#partition-the-drive"><span
class="toc-section-number">3.1</span> Partition the drive</a></li>
<li><a href="#format-the-partition"><span
class="toc-section-number">3.2</span> Format the partition</a></li>
<li><a href="#create-the-filesystem"><span
class="toc-section-number">3.3</span> Create the filesystem</a></li>
</ul></li>
<li><a href="#mountingunmounting-the-drive"><span
class="toc-section-number">4</span> Mounting/unmounting the
drive</a></li>
<li><a href="#references"><span class="toc-section-number">5</span>
References</a></li>
</ul>
</div>
<h1 data-number="1" id="why-pmount"><span
class="header-section-number">1</span> Why pmount</h1>
<p>There are many ways to set up how removable media is mounted (or even
auto-mounted) in linux. Out of those, <em>pmount</em> provides the
following advantages:</p>
<ul>
<li>users can mount drives</li>
<li>out-of-the-box LUKS (encrypted) support</li>
<li>no configuration, even for encrypted partitions</li>
</ul>
<p>These are my short notes on how to format a USB drive in a way that
is compatible with pmount.</p>
<h1 data-number="2" id="identifying-the-usb-drive"><span
class="header-section-number">2</span> Identifying the USB drive</h1>
<p><code>lsblk -f</code> shows all block devices in the system. If we
run it before inserting the drive, and then after, the inserted drive is
the new device in the output:</p>
<pre><code>$ lsblk -f
NAME   FSTYPE      FSVER LABEL       UUID                                 FSAVAIL FSUSE% MOUNTPOINT
sda
|-sda1
|-sda2 ext4        1.0   sddroot     38e24d86-4374-4042-aebe-eeaec2317b6f    5.5G    84% /
sdb</code></pre>
<p>We can also use <a href="https://linux.die.net/man/1/watch">watch
(1)</a> or <a href="https://github.com/lpenz/ogle/">ogle</a> (shameless
plug) to monitor the output of <code>lsblk</code> and get an immediate
feedback when the drive is detected.</p>
<h1 data-number="3" id="creating-the-luks-encrypted-partition"><span
class="header-section-number">3</span> Creating the luks-encrypted
partition</h1>
<p>All examples assume that an empty USB drive was inserted and
identified as <code>/dev/sdb</code>. The commands below are dangerous,
do not use them if you don’t understand what they are doing.</p>
<h2 data-number="3.1" id="partition-the-drive"><span
class="header-section-number">3.1</span> Partition the drive</h2>
<p>We can use <code>fdisk</code> or any other partitioning tool. An
example session:</p>
<pre><code>$ {b}sudo fdisk /dev/sdb{/b}
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
Created a new partition 1 of type &#39;Linux&#39; and of size 49 MiB.
Command (m for help): {b}w{/b}
The partition table has been altered.
Syncing disks.</code></pre>
<p>The partition type doesn’t matter much, it has no relation with the
file system in linux. Use <code>83</code> (Linux) if in doubt.</p>
<h2 data-number="3.2" id="format-the-partition"><span
class="header-section-number">3.2</span> Format the partition</h2>
<p>Use <em>cryptsetup luksFormat</em> to format the partition; we also
choose the passphrase in this step. Let’s create a variable with the
partition (<code>/dev/sdb1</code> in this example) and format it:</p>
<pre><code>$ {b}part=/dev/sdb1{/b}
$ {b}sudo cryptsetup luksFormat --label cryptflash &quot;$part&quot;{/b}
WARNING: Device /dev/sdb1 already contains a &#39;dos&#39; partition signature.
WARNING!
========
This will overwrite data on /dev/sdb1 irrevocably.
Are you sure? (Type &#39;yes&#39; in capital letters): {b}YES{/b}
Enter passphrase for /dev/sdb1: {b}mypassphrase{/b}

Verify passphrase: {b}mypassphrase{/b}</code></pre>
<h2 data-number="3.3" id="create-the-filesystem"><span
class="header-section-number">3.3</span> Create the filesystem</h2>
<p>This is a two-step process: we first “open” the luks partition,
creating a pseudo-device; and then we create the filesystem we want on
top of that. The filesystem layer doesn’t care if the device is an
abstract cryptographic engine or a real partition.</p>
<p>Let’s open the luks partition in the pseudo-device
<code>cryptdev</code>:</p>
<pre><code>$ {b}sudo cryptsetup luksOpen &quot;$part&quot; cryptdev{/b}
Enter passphrase for /dev/sdb1: {b}mypassphrase{/b}</code></pre>
<p>We now have to create the filesystem. The <a
href="https://linux.die.net/man/1/pmount">pmount manual</a> has a list
of supported filesystems. <em>ext4</em> is the most popular one for
non-removable drives, but it’s not great for drives that we want to use
in multiple systems, as the UIDs are hard-coded in the FS. That means
that we don’t have the proper permissions when we plug the drive in
other systems where our UID has a different numerical value. We’d then
have to use <em>sudo</em>, which nullifies one of the advantages of
<em>pmount</em>.</p>
<p>For this reason, we are using <a
href="https://en.wikipedia.org/wiki/Universal_Disk_Format">UDF</a> for
the partition:</p>
<pre><code>$ {b}sudo mkfs.udf -l cryptflash /dev/mapper/cryptdev{/b}
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
mkudffs: Warning: UDF filesystem on partition cannot be read on Apple systems</code></pre>
<p>At last, we close the luks layer with:</p>
<pre><code>$ {b}sudo cryptsetup luksClose cryptdev{/b}</code></pre>
<h1 data-number="4" id="mountingunmounting-the-drive"><span
class="header-section-number">4</span> Mounting/unmounting the
drive</h1>
<p>We could <em>luksOpen</em> the device and just use the regular
<em>mount</em> to access the files - but using pmount is much more
convenient, as it takes care of doing both steps and doesn’t require any
configuration or permissions.</p>
<p>All partitions in block devices pop up in
<code>/dev/disk/by-label/</code> as symbolic links to the underlying
<code>/dev</code> entry. We can simply use that path as the argument to
pmount to get the corresponding filesystem mounted at
<code>/media/</code>:</p>
<pre><code>$ {b}pmount /dev/disk/by-label/cryptflash{/b}</code></pre>
<p>To unmount:</p>
<pre><code>$ {b}pumount /media/disk_by-label_cryptflash/{/b}</code></pre>
<p>We can also create a simple script that does that when given the
label:</p>
<pre><code>#!/bin/bash

label=#dollar#{1?Usage $0 &lt;label&gt;}

set -e -x

pmount &quot;/dev/disk/by-label/$label&quot;
: mounted at &quot;/media/disk_by-label_#dollar#{label}&quot;</code></pre>
<h1 data-number="5" id="references"><span
class="header-section-number">5</span> References</h1>
<ul>
<li><a href="https://linux.die.net/man/8/cryptsetup"
class="uri">https://linux.die.net/man/8/cryptsetup</a></li>
<li><a href="https://linux.die.net/man/8/cryptmount"
class="uri">https://linux.die.net/man/8/cryptmount</a></li>
<li><a href="https://linux.die.net/man/1/pmount"
class="uri">https://linux.die.net/man/1/pmount</a></li>
<li><a href="https://linux.die.net/man/1/pumount"
class="uri">https://linux.die.net/man/1/pumount</a></li>
<li><a
href="https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/"
class="uri">https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/</a></li>
<li><a href="https://en.wikipedia.org/wiki/Universal_Disk_Format"
class="uri">https://en.wikipedia.org/wiki/Universal_Disk_Format</a></li>
<li><a
href="https://unix.stackexchange.com/questions/38309/different-uid-gid-when-using-an-ext4-formatted-usb-drive-with-another-computer">Different
UID/GID when using an ext4 formatted USB drive with another
computer</a>: why using ext4 on a USB drive is not a great idea</li>
</ul>
