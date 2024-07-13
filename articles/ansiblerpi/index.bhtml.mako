<div class="body" id="body">
<p>
These are my notes on how to get started provisioning Raspbian with
ansible.
</p>

<section>
<h1>Before using ansible</h1>

<p>
We have to get a working Raspbian installation before using ansible on
the device.
</p>

<section>
<h2>Installing Raspbian</h2>

<p>
The first step is downloading and installing Raspbian in the SD card
you're going to use in the Pi. You can find the Raspbian image here:
<a href="https://www.raspberrypi.org/downloads/raspbian/">https://www.raspberrypi.org/downloads/raspbian/</a>. Prefer the "lite"
image, as it allows you to start from a cleaner point.
</p>
<p>
To install the image, use an adapter to plug the SD card into a
computer. You can figure out the <code>/dev/</code> device that got assigned by
using <code>lsblk</code> before and after plugging the card. Let's assume it
got assigned to <code>/dev/sdc</code>, for the sake of illustration. The next
step is writting the Raspbian image to it:
</p>

<pre>
unzip -p *-raspbian-stretch-lite.zip | sudo dd of=/dev/sdc bs=4M oflag=dsync status=progress
</pre>

<p>
You can now insert the card into the Raspberry Pi board.
</p>

</section>
<section>
<h2>Initial Raspbian setup</h2>

<p>
Raspbian comes with a initial user <strong>pi</strong>, password <strong>raspberry</strong>,
that can use sudo freely.  <strong>ssh</strong> is installed, but
disabled. Changing the password and enabling ssh are the next steps,
in a console session that should look like the following:
</p>

<pre>
&gt; raspberrypi login: pi
&gt; Password:
passwd
&gt; Changing password for pi.
&gt; (current) UNIX password:
&gt; Enter new UNIX password:
&gt; Retype new UNIX password:
&gt; passwd: password updated successfully
sudo systemctl enable ssh.service
(...)
</pre>

</section>
<section>
<h2>Alternative initial setup</h2>

<p>
Another way to do the initial image setup is by mounting in your system and
changing the required files before writting it in the SD card. To mount the
image:
</p>

<ul>
<li>Extract it from the zip file:
 

<pre>
unzip *-raspbian-*-lite.zip
</pre>

 
</li>
<li>Using <code>losetup</code>, map the file to a loopback device, and list the loopback
  devices to figure out where it was mapped:
 

<pre>
sudo losetup -f *-raspbian-*-lite.img
sudo losetup -l
&gt; NAME       SIZELIMIT OFFSET AUTOCLEAR RO BACK-FILE             DIO LOG-SEC
&gt; /dev/loop0         0      0         0  0 *-raspbian-*-lite.img   0     512
</pre>

 
From here on we assume that the file was mapped to <code>/dev/loop0</code>.
 
</li>
<li>Tell the kernel to make the partitions available at <code>/dev/</code> using
  <code>partx</code>, figure out where they are using <code>lsblk</code>:
 

<pre>
sudo partx -a /dev/loop0
lsblk /dev/loop0
&gt; NAME      MAJ:MIN RM  SIZE RO TYPE MOUNTPOINT
&gt; loop0       7:0    0  1.8G  0 loop
&gt; ├─loop0p1 259:0    0 43.8M  0 loop
&gt; └─loop0p2 259:1    0  1.7G  0 loop
</pre>

 
</li>
<li>Mount the partition in a temporary directory:
 

<pre>
mkdir rpi
sudo mount /dev/loop0p2 rpi
</pre>

 
</li>
<li>You can now modify the files in the <code>rpi</code> directory. Use
  <code>openssl passwd -1 &lt;password&gt;</code> to get the string to use in the
  <code>etc/shadow</code> file as the password of the <code>pi</code> user. You can also change
  also change the name of the user, just be careful to change it everywhere
  needed:
  <ul>
  <li><code>/etc/passwd</code>
  </li>
  <li><code>/etc/shadow</code>
  </li>
  <li><code>/etc/subuid</code>
  </li>
  <li><code>/etc/group</code>
  </li>
  <li><code>/etc/gshadow</code>
  </li>
  <li><code>/etc/subgid</code>
  </li>
  <li>And rename <code>/home/pi</code>
  </li>
  </ul>
 
  To enable ssh, you should create init and systemd links:
 

<pre>
sudo ln -s /lib/systemd/system/ssh.service rpi/etc/systemd/system/sshd.service
sudo ln -s /lib/systemd/system/ssh.service rpi/etc/systemd/system/multi-user.target.wants/ssh.service
for d in rc2.d rc3.d rc4.d rc5.d; do (cd rpi/etc/$d; sudo ln -sf ../init.d/ssh S01ssh); done
find rpi/etc/rc* -name 'K*ssh' -exec sudo rm {} +
</pre>

 
  Other, security related things to do:
 

<pre>
sudo rm rpi/etc/sudoers.d/010_pi-nopasswd
</pre>

 
  You can also change other things if you want. When you are done, proceed to
  the next item.
 
</li>
<li>Unmount the partition, remove the temporary directory and unmap the loopback
  device:
 

<pre>
sudo umount rpi
rmdir rpi
sudo losetup -d /dev/loop0
</pre>

 
</li>
<li>Write the image to the SD card as before:
 

<pre>
sudo dd if=*-raspbian-*-lite.img of=/dev/sdc bs=4M oflag=dsync status=progress
</pre>

</li>
</ul>

</section>
</section>
<section>
<h1>Using ansible</h1>

<p>
You can now use ansible from any computer to provision the Pi. I'd
recommend, though, using <em>python3</em> as the interpreter on the Pi, as
it has more modules available - *apt_repository*, for instance, is not
directly usable with the default python2 interpreter.
</p>
<p>
A basic playbook that runs tasks as root looks like the following
(save it as <code>playbook-sudo-rpi.yml</code>):
</p>

<pre>
---
- hosts: all
  become: yes
  become_user: root
  vars:
    ansible_python_interpreter: /usr/bin/python3
  tasks:
    - debug: msg="Ansible running in {{ansible_lsb.id}}!"
</pre>

<p>
Assuming that the playbook as available as <code>playbook.yml</code>, and that
<code>raspberrypi</code> is a name that can be resolved to the device, you can
run the playbook with the following command:
</p>

<pre>
ansible-playbook -i raspberrypi, -u pi playbook-sudo-rpi.yml -K
</pre>

<p>
(you can also replace <code>raspberrypi</code> with the actual IP address of
the device)
</p>

<section>
<h2>Setting up Debian repositories</h2>

<p>
Because Raspbian is based on Debian and upstream Debian has support for the
architecture of the Raspberry Pi B+, you can configure and use upstream Debian
repositories and packages in the Pi. To do that, use the follwing snippet:
</p>

<pre>
(...)
  tasks:
    - name: install apt pinning config
      copy: src=apt-pinning dest=/etc/apt/preferences.d/99pinning owner=root group=root mode=0644
    - name: install Debian apt key
      apt_key: url='https://ftp-master.debian.org/keys/archive-key-9.asc' id='E1CF20DDFFE4B89E802658F1E0B11894F66AEC98' state=present
      notify: apt-update
    - name: install Debian apt repositories
      apt_repository: repo='deb http://ftp.debian.org/debian {{item}} main contrib non-free' state=present
      with_items:
        - stable
        - testing
        - unstable
        - experimental
      notify: apt-update
  handlers:
    - name: apt-update
      apt: update-cache=yes
</pre>

<p>
The <a href="apt-pinning">apt-pinning</a> file should contain something like the following:
</p>

<pre>
Package: *
Pin: release o=Raspberry Pi Foundation
Pin-Priority: 600

Package: *
Pin: release o=Raspbian
Pin-Priority: 600

Package: *
Pin: release a=stable
Pin-Priority: 510

Package: *
Pin: release a=testing
Pin-Priority: 520

Package: *
Pin: release a=unstable
Pin-Priority: 150

Package: *
Pin: release a=experimental
Pin-Priority: 120
</pre>

</section>
</section>
</div>