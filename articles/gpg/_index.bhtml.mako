<div id="toc">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#introduction"><span class="toc-section-number">1</span>
Introduction</a>
<ul>
<li><a href="#basics-of-public-key-infrastructure-pki"><span
class="toc-section-number">1.1</span> Basics of public key
infrastructure (PKI)</a></li>
<li><a href="#the-strategy"><span class="toc-section-number">1.2</span>
The strategy</a></li>
</ul></li>
<li><a href="#master-key-setup"><span
class="toc-section-number">2</span> Master key setup</a>
<ul>
<li><a href="#using-a-flash-drive"><span
class="toc-section-number">2.1</span> Using a flash drive</a></li>
<li><a href="#configuring-gpg"><span
class="toc-section-number">2.2</span> Configuring gpg</a></li>
<li><a href="#creating-the-master-key"><span
class="toc-section-number">2.3</span> Creating the master key</a></li>
<li><a href="#adding-a-secondary-id-and-email"><span
class="toc-section-number">2.4</span> Adding a secondary ID and
email</a></li>
<li><a href="#importing-the-master-public-key-in-the-host"><span
class="toc-section-number">2.5</span> Importing the master public key in
the host</a></li>
</ul></li>
<li><a href="#adding-a-subkey"><span class="toc-section-number">3</span>
Adding a subkey</a>
<ul>
<li><a href="#creating-the-subkey"><span
class="toc-section-number">3.1</span> Creating the subkey</a></li>
<li><a href="#importing-the-new-subkey-in-the-target-system"><span
class="toc-section-number">3.2</span> Importing the new subkey in the
target system</a></li>
</ul></li>
<li><a href="#using-the-keys"><span class="toc-section-number">4</span>
Using the keys</a>
<ul>
<li><a href="#configuring-git"><span
class="toc-section-number">4.1</span> Configuring git</a></li>
<li><a href="#setting-up-key-validation-in-service-providers"><span
class="toc-section-number">4.2</span> Setting up key validation in
service providers</a></li>
</ul></li>
<li><a href="#addendum-key-list-details"><span
class="toc-section-number">5</span> Addendum: key list details</a></li>
<li><a href="#references"><span class="toc-section-number">6</span>
References</a></li>
</ul>
</div>
<h1 data-number="1" id="introduction"><span
class="header-section-number">1</span> Introduction</h1>
<p>github, linux packages and repositories, email… a cryptographic
digital signature can be used in several places to provide
<em>authenticity</em> - a reasonable proof that the artifact was indeed
generate by the author.</p>
<p>These are my notes on gpg signature management. gpg has changed a lot
since I last used it, and I’ve decided to shift to a new strategy -
might as well write it all down this time.</p>
<p>The notation in this article uses <b>bold</b> for user input.</p>
<h2 data-number="1.1" id="basics-of-public-key-infrastructure-pki"><span
class="header-section-number">1.1</span> Basics of public key
infrastructure (PKI)</h2>
<p>In broad strokes, a digital signature works in the context of the
following workflow:</p>
<ul>
<li>In the past, the author generated in a linked pair of keys: a public
one and a private one. They are blobs of data, with the mathematical
property that whatever is encripted with the private key can only be
decrypted with the matching public key.</li>
<li>The author sends away the public key, registers it on services, etc.
(that’s why it’s called “public”)</li>
<li>To sign an artifact, the author calculates its hash, encrypts this
hash with the secret key, and attaches the hash to the artifact. The
encrypted hash is the digital signature.</li>
<li>To check the signature, a recipient calculates the artifact’s hash
the same way the author did, and decrypts the received hash using the
author’s well-known public key. The match between the calculated and
decrypted hashes implies two things:
<ul>
<li>That the artifact was not tampered;</li>
<li>That the received hash was encrypted with the secret key that
matches the public key of that author.</li>
</ul></li>
</ul>
<p>The <a
href="https://en.wikipedia.org/wiki/Digital_signature">wikipedia
page</a> has more details.</p>
<h2 data-number="1.2" id="the-strategy"><span
class="header-section-number">1.2</span> The strategy</h2>
<p>Using gpg to generate a pair of cryptographic keys for digital
signatures is quite trivial. That’s not what we are going to do.</p>
<p>Our strategy, instead, is to generate a <em>master</em> key pair that
we then use to generate several signature subkey pairs, that we can then
use in different hosts and for different services. After generating the
keys, we put the master key away, and leave installed in each host only
the relevant secret keys. This has some advatanges:</p>
<ul>
<li>Minimizes the exposure of the master key, as it’s not used on a
daily basis and is not permanently installed in any host.</li>
<li>Limits what an attacker can do with a secret key that gets
compromised - the secret key and the embedded information can’t be
changed without the master key. In contrast, a compromised top-level key
can be altered by itself.</li>
<li>When a host is compromised, you know exactly which keys are
compromised and have to be revoked, and which services have to be
updated, because that’s documented in the keys themselves.</li>
</ul>
<p>Keep in mind that all these points depend on the security of the
master key.</p>
<h1 data-number="2" id="master-key-setup"><span
class="header-section-number">2</span> Master key setup</h1>
<p>The instruction in this section are the initial setup and master key
creation. It should be done only once.</p>
<h2 data-number="2.1" id="using-a-flash-drive"><span
class="header-section-number">2.1</span> Using a flash drive</h2>
<p>Part of our strategy involves keeping the master key secure. One way
of doing that is by keeping it in a flash drive that is physically kept
secure. We could create and work with the key locally and then move it
away, but we are instead going to work with it straight from a flash
drive.</p>
<p>The master key is already secured by a password, so there’s no need
to encrypt the flash drive because of it. If you want to keep other
sensitive files there, though, you should encrypt it. You can take a
look at the <a href="../luksfile/index.html">Creating an encrypted
directory-in-a-file</a> article for basic instructions.</p>
<p>In my particular setup, I’m using a flash drive with a
luks2-encrypted partition labeled <em>cryptflash</em> that I mount
with:</p>
<pre><code>$ <b>cd /dev/disk/by-label</b>
$ <b>pmount cryptflash</b>
Enter passphrase for cryptflash: <b>mypassphrase</b></code></pre>
<p>Note that <code>pmount</code> doesn’t need any configuration to do
that - it detects luks and the filesystem, and mounts that partition in
<code>/media/cryptflash</code>. We assume this path is being used
below.</p>
<h2 data-number="2.2" id="configuring-gpg"><span
class="header-section-number">2.2</span> Configuring gpg</h2>
<p>It’s worth noting that the ages-old interface design of gpg doesn’t
support this approach in an intuitive way with the default
configuration. The first thing we should do is add a couple of lines to
<em>~/.gnupg/gpg.conf</em>:</p>
<pre><code>utf8-strings
keyid-format long
with-fingerprint
with-sig-list
list-options show-notations
</code></pre>
<p>These options make gpg show more information about the subkeys,
information we are going to set and use.</p>
<h2 data-number="2.3" id="creating-the-master-key"><span
class="header-section-number">2.3</span> Creating the master key</h2>
<p>We start by assigning the directory in the detachable drive to
<code>GNUPGHOME</code>:</p>
<pre><code>$ <b>export GNUPGHOME=&quot;/media/cryptflash/dotgpg&quot;</b></code></pre>
<p>We create the directory and copy our <code>~/.gnupg/gnupg.conf</code>
to it:</p>
<pre><code>$ <b>mkdir -p &quot;$GNUPGHOME&quot;</b>
$ <b>cp &quot;$HOME/.gnupg/gpg.conf&quot; &quot;$GNUPGHOME/&quot;</b></code></pre>
<p>To keep the instructions below a bit more “copy-pasteable”, let’s use
an <code>EMAIL</code> variable and the following <code>FULLNAME</code>
variable:</p>
<pre><code>$ <b>FULLNAME=$(getent passwd &quot;$USER&quot; | sed -nE &#39;s@^([^:]+:){4}([^,:]+).*@\2@p&#39;)</b></code></pre>
<p>We can now generate the master key pair:</p>
<pre><code>$ <b>gpg --quick-gen-key &quot;$FULLNAME &lt;$EMAIL&gt;&quot; default sign 0</b>
gpg: keybox &#39;pubring.kbx&#39; created
We need to generate a lot of random bytes. It is a good idea to perform
some other action (type on the keyboard, move the mouse, utilize the
disks) during the prime generation; this gives the random number
generator a better chance to gain enough entropy.
Enter passphrase: <b>mypassphrase</b>
gpg: trustdb.gpg: trustdb created
gpg: directory &#39;openpgp-revocs.d&#39; created
gpg: revocation certificate stored as &#39;openpgp-revocs.d/9E463562FF6DE4D16594C0C940B511B04A0B8FCC.rev&#39;
public and secret key created and signed.
Note that this key cannot be used for encryption.  You may want to use
the command &quot;--edit-key&quot; to generate a subkey for this purpose.
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                            Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
</code></pre>
<p>That command asks you for a password, and then creates the master key
pair with default options and no expiration date. For more details on
why a master key expiration date is irrelevant in our scenario, read <a
href="https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/">this</a>.</p>
<p>Keep in mind that gpg’s interface is… weird. It has its own REPL that
you can use if you want. In this article we are always invoking it from
the shell and passing the commands as arguments.</p>
<p>We’ll use <code>gpg -k</code> to lists the public keys and their
states quite often as we create/edit keys in this article, starting
now:</p>
<pre><code>$ <b>gpg -k</b>
gpg: checking the trustdb
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u
pubring.kbx
----------------------------
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
</code></pre>
<p>The <code>40B511B04A0B8FCC</code> above is the generated ID of our
master key and we’ll need it every time want to use the key. We might as
well store it in a variable:</p>
<pre><code>$ <b>masterkeyid=40B511B04A0B8FCC</b></code></pre>
<h2 data-number="2.4" id="adding-a-secondary-id-and-email"><span
class="header-section-number">2.4</span> Adding a secondary ID and
email</h2>
<p>If you have a secondary email, you should add another User ID:</p>
<pre><code>$ <b>gpg --quick-add-uid &quot;$FULLNAME &lt;$EMAIL&gt;&quot; &quot;$FULLNAME &lt;llpenz@gmail.com&gt;&quot;</b>
Enter passphrase: <b>mypassphrase</b>
</code></pre>
<p>Notice that, by default, the last user ID becomes the primary and
it’s trust status is <code>unknown</code>:</p>
<pre><code>$ <b>gpg -k</b>
pubring.kbx
----------------------------
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ unknown] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
</code></pre>
<p>To make the old user ID the primary, select it with
<code>uid 1</code> and invoke <code>primary</code>; then run
<code>gpg --check-trustdb</code> to update the trust status of the new
user ID:</p>
<pre><code>$ <b>gpg --edit-key &quot;$masterkeyid&quot; uid 2 primary save</b>
gpg (GnuPG) 2.2.40; Copyright (C) 2022 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Secret key is available.
sec  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1). Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
[ultimate] (2)  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sec  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1). Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
[ultimate] (2)  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sec  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1). Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
[ultimate] (2)* Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sec  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
[ultimate] (2)* Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
$ <b>gpg --check-trustdb</b>
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u
</code></pre>
<p>These last 2 lines are probably familiar: sometimes,
<code>gpg -k</code> updates the trusted database, but not always.</p>
<p>We now have the 2 user IDs in our master key
<code>40B511B04A0B8FCC</code>, in the correct order:</p>
<pre><code>$ <b>gpg -k</b>
pubring.kbx
----------------------------
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
</code></pre>
<p>To see the corresponding private keys, use <code>gpg -K</code>
(capital):</p>
<pre><code>$ <b>gpg -K</b>
pubring.kbx
----------------------------
sec   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
</code></pre>
<p>The notable difference above is the <em>sec</em> keyword describing
the secret part of our master key pair.</p>
<h2 data-number="2.5"
id="importing-the-master-public-key-in-the-host"><span
class="header-section-number">2.5</span> Importing the master public key
in the host</h2>
<p>We can now export the <strong>public</strong> part of the master key
pair, and import it in the host system. That allows us to check the
signatures of our subkeys and give them trust.</p>
<pre><code>$ <b>gpg --armor --output /tmp/master.pub.gpg --export &quot;$masterkeyid&quot;</b>
</code></pre>
<p>We don’t actually need access to the master key flash drive after
exporting its public part. We can then reset <code>GNUPGHOME</code>.
That makes <code>gpg</code> use the default database, to where we import
the file:</p>
<pre><code>$ <b>unset GNUPGHOME</b>
$ <b>gpg --import /tmp/master.pub.gpg</b>
gpg: keybox &#39;pubring.kbx&#39; created
gpg: trustdb.gpg: trustdb created
gpg: key 40B511B04A0B8FCC: public key &quot;Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;&quot; imported
gpg: Total number processed: 1
gpg:               imported: 1
</code></pre>
<p>And trust it:</p>
<pre><code>$ <b>gpg --edit-key &quot;$masterkeyid&quot; trust save</b>
gpg (GnuPG) 2.2.40; Copyright (C) 2022 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
pub  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: unknown       validity: unknown
[ unknown] (1). Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
[ unknown] (2)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
pub  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: unknown       validity: unknown
[ unknown] (1). Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
[ unknown] (2)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
Please decide how far you trust this user to correctly verify other users&#39; keys
(by looking at passports, checking fingerprints from different sources, etc.)
  1 = I don&#39;t know or won&#39;t say
  2 = I do NOT trust
  3 = I trust marginally
  4 = I trust fully
  5 = I trust ultimately
  m = back to the main menu
Your decision? <b>5</b>
Do you really want to set this key to ultimate trust? (y/N) <b>y</b>
pub  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: unknown
[ unknown] (1). Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
[ unknown] (2)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
Please note that the shown key validity is not necessarily correct
unless you restart the program.
Key not changed so no update needed.
</code></pre>
<p>Result:</p>
<pre><code>$ <b>gpg -k</b>
gpg: checking the trustdb
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u
pubring.kbx
----------------------------
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
</code></pre>
<p>As we have imported only the public part of the master key pair,
<code>gpg -K</code> won’t show us anything:</p>
<pre><code>$ <b>gpg -K</b>
</code></pre>
<p>And this is the setup. After it is done, we can do the following to
get the flash drive unmounted:</p>
<pre><code>$ <b>pumount cryptflash</b></code></pre>
<p>We can now remove the flash drive. Keep it safe.</p>
<h1 data-number="3" id="adding-a-subkey"><span
class="header-section-number">3</span> Adding a subkey</h1>
<p>The single purpose of the master key is the generation of subkeys -
one for each combination of host and service, in a matrix-like fashion.
That allows us to track down all services affected by a vulnerable host,
and act accordingly.</p>
<p>First, we insert the master key flash drive, mount it and set
GNUPGHOME:</p>
<pre><code>$ <b>cd /dev/disk/by-label</b>
$ <b>pmount cryptflash</b>
Enter passphrase for cryptflash: <b>mypassphrase</b>
$ <b>export GNUPGHOME=&quot;/media/cryptflash/dotgpg&quot;</b></code></pre>
<h2 data-number="3.1" id="creating-the-subkey"><span
class="header-section-number">3.1</span> Creating the subkey</h2>
<p>Before adding the subkey, we have to decide what we are going to use
it for so that we can put this information inside a <em>notation</em>.
Notations are key-value tags assigned to a signature, where the key has
the format <code>id@domain</code>, and <em>domain</em> acts as a
namespace - more information in <a
href="https://tools.ietf.org/html/rfc4880#section-5.2.3.16">RFC4880</a>.
We can also use a second notation to identify the host where the key is
installed. I’m actually using <code>host@lpenz.org</code> for the hosts
and <code>service@lpenz.org</code> for the services.</p>
<p>So, to add a subkey for <em>github</em> that is installed in the host
<em>darkstar</em>:</p>
<pre><code>$ <b>gpg --cert-notation host@lpenz.org=darkstar --cert-notation service@lpenz.org=github --edit-key &quot;$masterkeyid&quot; addkey save</b>
gpg (GnuPG) 2.2.40; Copyright (C) 2022 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Secret key is available.
sec  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ultimate] (1). Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
[ultimate] (2)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
Please select what kind of key you want:
   (3) DSA (sign only)
   (4) RSA (sign only)
   (5) Elgamal (encrypt only)
   (6) RSA (encrypt only)
  (14) Existing key from card
Your selection? <b>4</b>
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (3072) <b></b>
Requested keysize is 3072 bits
Please specify how long the key should be valid.
         0 = key does not expire
      &lt;n&gt;  = key expires in n days
      &lt;n&gt;w = key expires in n weeks
      &lt;n&gt;m = key expires in n months
      &lt;n&gt;y = key expires in n years
Key is valid for? (0) <b></b>
Key does not expire at all
Is this correct? (y/N) <b>y</b>
Really create? (y/N) <b>y</b>
Enter passphrase: <b>mypassphrase</b>
We need to generate a lot of random bytes. It is a good idea to perform
some other action (type on the keyboard, move the mouse, utilize the
disks) during the prime generation; this gives the random number
generator a better chance to gain enough entropy.
sec  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
ssb  rsa3072/DCB4FF233FA79E0E
     created: 2024-06-30  expires: never       usage: S
[ultimate] (1). Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
[ultimate] (2)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
</code></pre>
<h2 data-number="3.2"
id="importing-the-new-subkey-in-the-target-system"><span
class="header-section-number">3.2</span> Importing the new subkey in the
target system</h2>
<p>After creating the new subkey in the master key flash drive, we have
to export the pair and then import it in the host where it will be
used.</p>
<pre><code>$ <b>gpg --armor --output /tmp/newkey.sec.gpg --export-secret-subkey DCB4FF233FA79E0E!</b>
Enter passphrase: <b>mypassphrase</b>
</code></pre>
<p>After exporting the file, we unset <code>GNUPGHOME</code> and import
the file into the default database:</p>
<pre><code>$ <b>unset GNUPGHOME</b>
$ <b>gpg --import /tmp/newkey.sec.gpg</b>
gpg: key 40B511B04A0B8FCC: &quot;Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;&quot; 1 new signature
gpg: key 40B511B04A0B8FCC: &quot;Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;&quot; 1 new subkey
Enter passphrase: <b>mypassphrase</b>
gpg: To migrate &#39;secring.gpg&#39;, with each smartcard, run: gpg --card-status
gpg: key 40B511B04A0B8FCC: secret key imported
gpg: Total number processed: 1
gpg:            new subkeys: 1
gpg:         new signatures: 1
gpg:       secret keys read: 1
gpg:   secret keys imported: 1
</code></pre>
<p>Results:</p>
<pre><code>$ <b>gpg -k</b>
pubring.kbx
----------------------------
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sub   rsa3072/DCB4FF233FA79E0E 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github

$ <b>gpg -K</b>
pubring.kbx
----------------------------
sec#  rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
ssb   rsa3072/DCB4FF233FA79E0E 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github
</code></pre>
<p>We can check that we can’t add a new subkey without using
<code>cryptflash</code> as <code>GNUPGHOME</code>:</p>
<pre><code>$ <b>gpg --edit-key &quot;$masterkeyid&quot; addkey save</b>
gpg (GnuPG) 2.2.40; Copyright (C) 2022 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Secret subkeys are available.
pub  rsa3072/40B511B04A0B8FCC
     created: 2024-06-30  expires: never       usage: SC
     trust: ultimate      validity: ultimate
ssb  rsa3072/DCB4FF233FA79E0E
     created: 2024-06-30  expires: never       usage: S
[ultimate] (1). Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
[ultimate] (2)  Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
Need the secret key to do this.
Key not changed so no update needed.
</code></pre>
<p>We can also test that the signature still works with by signing a
“test” message:</p>
<pre><code>$ <b>echo &quot;test&quot; | gpg --clearsign</b>
-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA512
test
Enter passphrase: <b>mypassphrase</b>
-----BEGIN PGP SIGNATURE-----
iQGzBAEBCgAdFiEEsk+jlTVT5rAcB/Px3LT/Iz+nng4FAmaBv/kACgkQ3LT/Iz+n
ng4B0wwAs59mU05MCWnYKiOyENeZ3hAw+t0CzmEDnInzNnOEl5+1NmuexSeA1445
R8UhuNVJmAXuOvE196rXeSoSGszdSvE4PSNHx24jP01tzkhwhQYfFokAjQVjpfX2
lZdLoAZG1nLXD3QoTx0p2O5V3vS6wJ4Glq/j92tH12GdaDKqw1HC55FIOpenKP5i
CfwFFtV1+olm+L5NWSRNhL3/OegrCU8HGxOMfzVkWyPvf36VxYGkoN6ZzHfhK3h9
RgGubB22+w/hcTkBQ6hKfswTC3ZyZ7V1DSp2i3FJQly2jE279E1BeKblt/p5+dHy
Z+OR/KH0m1kkwKAr1OpLgfMqxIzEAmh4e1ammJh0UOb44Zv1PZHcxQq9qJaI40ie
LArHIAJ9w02hV+8y9F+Rqj4R2wTXrVKsaxWJcr9JCGjYRVTgOoVzcP0jlr7HeUHn
HBS0o1MYlIJ1FUBkl1mp53wBoP7V7LyvYYoL3FZdUzJ9IfAOb+lcr0YNdqFqyIKL
tMV5TlMI
=fHwJ
-----END PGP SIGNATURE-----
</code></pre>
<h1 data-number="4" id="using-the-keys"><span
class="header-section-number">4</span> Using the keys</h1>
<h2 data-number="4.1" id="configuring-git"><span
class="header-section-number">4.1</span> Configuring git</h2>
<p>To configure your signing key in git:</p>
<pre><code>$ <b>git config --global user.signingkey DCB4FF233FA79E0E</b></code></pre>
<p>To sign a commit, use <code>-S</code>:</p>
<pre><code>$ <b>git commit -S -m &#39;Add a commit message here&#39;</b>
[main (root-commit) ed1fad9] Add a commit message here
 1 file changed, 1 insertion(+)
 create mode 100644 test.txt
</code></pre>
<p>And we can see the signature with <code>--show-signature</code>:</p>
<pre><code>$ <b>git log -1 --show-signature</b>
commit ed1fad964ca5ba5be114fffe5d69735ee828a0b8 (HEAD -&gt; main)
gpg: Signature made Sun Jun 30 20:28:42 2024 UTC
gpg:                using RSA key B24FA3953553E6B01C07F3F1DCB4FF233FA79E0E
gpg: Good signature from &quot;Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;&quot; [ultimate]
gpg:                 aka &quot;Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;&quot; [ultimate]
Primary key fingerprint: 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
     Subkey fingerprint: B24F A395 3553 E6B0 1C07  F3F1 DCB4 FF23 3FA7 9E0E
Author: Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
Date:   Sun Jun 30 20:28:42 2024 +0000
    Add a commit message here
</code></pre>
<p>We can also configure git to sign all commits by default:</p>
<pre><code>$ <b>git config --global commit.gpgsign true</b></code></pre>
<h2 data-number="4.2"
id="setting-up-key-validation-in-service-providers"><span
class="header-section-number">4.2</span> Setting up key validation in
service providers</h2>
<p>One need the public key to be able to validate the corresponding
private key signature. To get that in a format that can be configured in
most services, we use:</p>
<pre><code>$ <b>gpg --armor --export DCB4FF233FA79E0E</b>
-----BEGIN PGP PUBLIC KEY BLOCK-----
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
0FO6v6myJ+hLiJl7JTPLyVN1fm8=
=ro4+
-----END PGP PUBLIC KEY BLOCK-----
</code></pre>
<p>We then copy that to the clipboard, and paste in the settings page of
the services. Here we have the instructions for some git hosting
services:</p>
<ul>
<li>GitHub: <a
href="https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account"
class="uri">https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account</a></li>
<li>GitLab: <a
href="https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account"
class="uri">https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account</a></li>
<li>Bitbucket: <a
href="https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add"
class="uri">https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add</a></li>
</ul>
<h1 data-number="5" id="addendum-key-list-details"><span
class="header-section-number">5</span> Addendum: key list details</h1>
<p>To list the public keys:</p>
<pre><code>$ <b>gpg -k</b>
pubring.kbx
----------------------------
pub   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sub   rsa3072/DCB4FF233FA79E0E 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github
sub   rsa3072/AFE62059EB06CB1E 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=github
sub   rsa3072/099CBC275805D95C 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=email
sub   rsa3072/9109DC57BDA82311 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=github
sub   rsa3072/1AC2EE140B4022FC 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=email
</code></pre>
<p>To list the private keys:</p>
<pre><code>$ <b>gpg -K</b>
pubring.kbx
----------------------------
sec   rsa3072/40B511B04A0B8FCC 2024-06-30 [SC]
      Key fingerprint = 9E46 3562 FF6D E4D1 6594  C0C9 40B5 11B0 4A0B 8FCC
uid                 [ultimate] Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
uid                 [ultimate] Leandro Lisboa Penz &lt;llpenz@gmail.com&gt;
sig 3        40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
ssb   rsa3072/DCB4FF233FA79E0E 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github
ssb   rsa3072/AFE62059EB06CB1E 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=github
ssb   rsa3072/099CBC275805D95C 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=email
ssb   rsa3072/9109DC57BDA82311 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=github
ssb   rsa3072/1AC2EE140B4022FC 2024-06-30 [S]
sig      N   40B511B04A0B8FCC 2024-06-30  Leandro Lisboa Penz &lt;lpenz@lpenz.org&gt;
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=email
</code></pre>
<p>In the output above:</p>
<ul>
<li><em>pub</em> identifies a public primary key entry.</li>
<li><em>sub</em> identifies a public subkey entry.</li>
<li><em>sec</em> identifies a private key entry, and the <em>#</em>
marks the key as absent (we are not looking at the USB drive).</li>
<li><em>ssb</em> identifies a private subkey entry.</li>
<li><em>uid</em> and <em>sig</em> show information about the current key
entry.</li>
<li><em>N</em> means the key has notations, which are shown below.</li>
</ul>
<h1 data-number="6" id="references"><span
class="header-section-number">6</span> References</h1>
<ul>
<li>Why expiration dates are irrelevant: <a
href="https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/"
class="uri">https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/</a></li>
<li>Ana Guerrero López tutorial that uses subkeys: <a
href="http://ekaia.org/blog/2009/05/10/creating-new-gpgkey/"
class="uri">http://ekaia.org/blog/2009/05/10/creating-new-gpgkey/</a></li>
<li>There’s also a Debian page about subkeys: <a
href="https://wiki.debian.org/Subkeys"
class="uri">https://wiki.debian.org/Subkeys</a></li>
<li>Generating a revocation certificate with gpg: <a
href="https://debian-administration.org/article/450/Generating_a_revocation_certificate_with_gpg"
class="uri">https://debian-administration.org/article/450/Generating_a_revocation_certificate_with_gpg</a></li>
<li>Getting information from an armored gpg public key file: <a
href="https://superuser.com/questions/173417/getting-information-from-an-armored-gpg-public-key-file"
class="uri">https://superuser.com/questions/173417/getting-information-from-an-armored-gpg-public-key-file</a></li>
<li>Secure yourself, Part 1: Air-gapped computer, GPG and smartcards: <a
href="https://viccuad.me/blog/Revisited-secure-yourself-part-1-airgapped-computer-and-gpg-smartcards"
class="uri">https://viccuad.me/blog/Revisited-secure-yourself-part-1-airgapped-computer-and-gpg-smartcards</a></li>
<li><a
href="https://tools.ietf.org/html/rfc4880#section-5.2.3.16">RFC4880</a></li>
<li>Details about listing format: <a
href="https://github.com/gpg/gnupg/blob/master/doc/DETAILS"
class="uri">https://github.com/gpg/gnupg/blob/master/doc/DETAILS</a></li>
<li>Signatures in git: <a
href="https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work"
class="uri">https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work</a></li>
<li>Adding the public GPG key to providers:
<ul>
<li>GitHub: <a
href="https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account"
class="uri">https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account</a></li>
<li>GitLab: <a
href="https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account"
class="uri">https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account</a></li>
<li>Bitbucket: <a
href="https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add"
class="uri">https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add</a></li>
</ul></li>
</ul>
