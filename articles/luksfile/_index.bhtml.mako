<div id="toc">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#introduction"><span class="toc-section-number">1</span>
Introduction</a></li>
<li><a href="#file-creation"><span class="toc-section-number">2</span>
File creation</a></li>
<li><a href="#testing-by-mounting"><span
class="toc-section-number">3</span> Testing by mounting</a></li>
<li><a href="#setting-up-user-mount"><span
class="toc-section-number">4</span> Setting up user mount</a></li>
<li><a href="#mounting"><span class="toc-section-number">5</span>
Mounting</a></li>
<li><a href="#final-remarks"><span class="toc-section-number">6</span>
Final remarks</a></li>
<li><a href="#references"><span class="toc-section-number">7</span>
References</a></li>
</ul>
</div>
<h1 data-number="1" id="introduction"><span
class="header-section-number">1</span> Introduction</h1>
<p>LUKS can be used to create a file with a whole encrypted filesystem
inside, that we can mount and manipulate in-place.</p>
<p>This article has my notes on how to create the file and set up the
mounting system.</p>
<p>Obs: to ease copy-and-pasting, we show the commands without prompt,
and prepend <code>&gt;</code> to the output of commands in the
examples.</p>
<h1 data-number="2" id="file-creation"><span
class="header-section-number">2</span> File creation</h1>
<p>We’ll use <strong>cryptsetup</strong> to create the file, it’s
available in all major distributions.</p>
<p>First, create a file with the desired size (64MB in this case):</p>
<pre><code>dd if=/dev/zero of=cryptfile.img bs=1M count=64
&gt; 64+0 records in
&gt; 64+0 records out
&gt; 67108864 bytes (67 MB, 64 MiB) copied, 1.36272 s, 49.2 MB/s</code></pre>
<p>Note: be aware that ext4 doesn’t support devices with less than
32MB</p>
<p>Use <em>cryptsetup luksFormat</em> to format it; we also choose the
passphrase in this step:</p>
<pre><code>sudo cryptsetup luksFormat cryptfile.img
&gt;
&gt; WARNING!
&gt; ========
&gt; This will overwrite data on cryptfile.img irrevocably.
&gt; 
&gt; Are you sure? (Type &#39;yes&#39; in capital letters): YES
&gt; Enter passphrase for cryptfile.img:
&gt; Verify passphrase:</code></pre>
<p>Use <em>cryptsetup luksOpen</em> to bind the file to a block device
that can be manipulated as if it was a partition:</p>
<pre><code>sudo cryptsetup luksOpen cryptfile.img cryptdev
&gt; Enter passphrase for cryptfile.img:</code></pre>
<p>After <em>cryptsetup luksOpen</em>, we are able to access the new
block device at <code>/dev/mapper/cryptdev</code>. It’s now time to
create the filesystem structure:</p>
<pre><code>sudo mkfs.ext4 /dev/mapper/cryptdev
&gt; mke2fs 1.45.5 (07-Jan-2020)
&gt; Creating filesystem with 49152 1k blocks and 12288 inodes
&gt; Filesystem UUID: c08602c2-acf7-4acf-b007-dce1200067d7
&gt; Superblock backups stored on blocks:
&gt;            8193, 24577, 40961
&gt; 
&gt; Allocating group tables: done
&gt; Writing inode tables: done
&gt; Creating journal (4096 blocks): done
&gt; Writing superblocks and filesystem accounting information: done</code></pre>
<p>We can now finally close the device</p>
<pre><code>sudo cryptsetup luksClose cryptdev</code></pre>
<h1 data-number="3" id="testing-by-mounting"><span
class="header-section-number">3</span> Testing by mounting</h1>
<p>To mount the file, first get the block device again:</p>
<pre><code>sudo cryptsetup luksOpen cryptfile.img cryptdev
&gt; Enter passphrase for cryptfile.img:</code></pre>
<p>And then mount as usual:</p>
<pre><code>mkdir -p cryptdir
sudo mount -t auto /dev/mapper/cryptdev cryptdir</code></pre>
<p>We always end up with a <em>lost+found</em> directory when we use the
usual linux filesystems:</p>
<pre><code>ls -l cryptdir/
&gt; total 12
&gt; drwx------ 2 root root 12288 Mar 22 17:47 lost+found</code></pre>
<p>Don’t forget to unmount and close the device after using it:</p>
<pre><code>sudo umount cryptdir
sudo cryptsetup luksClose cryptdev</code></pre>
<h1 data-number="4" id="setting-up-user-mount"><span
class="header-section-number">4</span> Setting up user mount</h1>
<p><strong>cryptmount</strong> allows us to mount configured encrypted
files without root permissions.</p>
<p>Create a <em>/etc/cryptmount/cmtab</em> file with the following
contents:</p>
<pre><code>mycryptlabel {
    dev=$(HOME)/cryptfile.img
    dir=$(HOME)/cryptdir
    fstype=auto
    keyformat=luks
}</code></pre>
<p>cryptmount is now able to mount the file.</p>
<h1 data-number="5" id="mounting"><span
class="header-section-number">5</span> Mounting</h1>
<p>Before mounting, you may want to go into a private mount
namespace:</p>
<pre><code>sudo unshare -m sudo -u &quot;$USER&quot; -i
&gt; [sudo] password for $USER:</code></pre>
<p>That starts a new shell in a new private mount namespace. Everything
you mount from this shell it will be available only to processes started
from this shell, and all mounts will be automatically unmounted when the
last of these processes exit.</p>
<p>You can now run the following command to mount the file:</p>
<pre><code>cryptmount mycryptlabel
&gt; Enter password for target &quot;mycryptlabel&quot;:
&gt; e2fsck 1.45.5 (07-Jan-2020)
&gt; /dev/mapper/cryptdev: clean, 33/4096 files, 1896/16384 blocks</code></pre>
<p>Don’t forget to unmount it later, and leave the shell:</p>
<pre><code>cryptmount -u mycryptlabel
exit</code></pre>
<h1 data-number="6" id="final-remarks"><span
class="header-section-number">6</span> Final remarks</h1>
<p>It should be said that these notes only touch the surface of
cryptsetup and cryptmount. They have a lot of options and can be used in
many different ways.</p>
<p>One interesting option is to keep the cryptographic key in a separate
file. That can be useful if you want to keep a rather large mountable
file in an exposed backed-up piece of infrastructure while minimizing
exposure.</p>
<p>All of these commands can be used to create and mount an encrypted
partition, just replace <em>cryptfile.img</em> with the path to the
partition block device. You can also create an encrypted partition that
encompasses an entire flash drive, leaving it without a partition table
at all, by using the appropriate block device.</p>
<h1 data-number="7" id="references"><span
class="header-section-number">7</span> References</h1>
<ul>
<li><a href="https://linux.die.net/man/8/cryptsetup"
class="uri">https://linux.die.net/man/8/cryptsetup</a></li>
<li><a href="https://linux.die.net/man/8/cryptmount"
class="uri">https://linux.die.net/man/8/cryptmount</a></li>
<li><a
href="https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/"
class="uri">https://willhaley.com/blog/encrypted-file-container-disk-image-in-linux/</a></li>
</ul>
