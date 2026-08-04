---
title: Using GPG and signing git commits
subtitle: Comment those subkeys
date: 2022-05-22
...


# Introduction

github, linux packages and repositories, email... a cryptographic
digital signature can be used in several places to
provide *authenticity* - a reasonable proof that the artifact was
indeed generate by the author.

These are my notes on gpg signature management. gpg has changed a lot
since I last used it, and I've decided to shift to a new strategy -
might as well write it all down this time.

The notation in this article uses {b}bold{/b} for user input.


## Basics of public key infrastructure (PKI)

In broad strokes, a digital signature works in the context of the
following workflow:

- In the past, the author generated in a linked pair of keys: a public
  one and a private one. They are blobs of data, with the mathematical
  property that whatever is encripted with the private key can only be
  decrypted with the matching public key.
- The author sends away the public key, registers it on services, etc.
  (that's why it's called "public")
- To sign an artifact, the author calculates its hash, encrypts this
  hash with the secret key, and attaches the hash to the artifact.
  The encrypted hash is the digital signature.
- To check the signature, a recipient calculates the artifact's hash
  the same way the author did, and decrypts the received hash using
  the author's well-known public key. The match between the calculated
  and decrypted hashes implies two things:
  - That the artifact was not tampered;
  - That the received hash was encrypted with the secret key that
    matches the public key of that author.


The [wikipedia page](https://en.wikipedia.org/wiki/Digital_signature)
has more details.



## The strategy

Using gpg to generate a pair of cryptographic keys for digital
signatures is quite trivial. That's not what we are going to do.

Our strategy, instead, is to generate a *master* key pair that we
then use to generate several signature subkey pairs, that we can then
use in different hosts and for different services. After generating
the keys, we put the master key away, and leave installed in each host
only the relevant secret keys. This has some advatanges:

- Minimizes the exposure of the master key, as it's not used on a
  daily basis and is not permanently installed in any host.
- Limits what an attacker can do with a secret key that gets
  compromised - the secret key and the embedded information can't be
  changed without the master key.
  In contrast, a compromised top-level key can be altered by itself.
- When a host is compromised, you know exactly which keys are
  compromised and have to be revoked, and which services have to be
  updated, because that's documented in the keys themselves.


Keep in mind that all these points depend on the security of the
master key.



# Master key setup

The instruction in this section are the initial setup and master key
creation. It should be done only once.


## Using a flash drive

Part of our strategy involves keeping the master key secure. One way
of doing that is by keeping it in a flash drive that is physically
kept secure. We could create and work with the key locally and then
move it away, but we are instead going to work with it straight from
a flash drive.

The master key is already secured by a password, so there's no need to
encrypt the flash drive because of it. If you want to keep other
sensitive files there, though, you should encrypt it. You can take a
look at the
[Creating an encrypted directory-in-a-file](../luksfile/index.html)
article for basic instructions.

In my particular setup, I'm using a flash drive with a luks2-encrypted
partition labeled *cryptflash* that I mount with:

```
$ {b}cd /dev/disk/by-label{/b}
$ {b}pmount cryptflash{/b}
Enter passphrase for cryptflash: {b}mypassphrase{/b}
```

Note that `pmount` doesn't need any configuration to do that - it
detects luks and the filesystem, and mounts that partition in
`/media/cryptflash`. We assume this path is being used below.


## Configuring gpg

It's worth noting that the ages-old interface design of gpg doesn't
support this approach in an intuitive way with the default
configuration. The first thing we should do is add a couple of lines
to *~/.gnupg/gpg.conf*:

```
utf8-strings
keyid-format long
with-fingerprint
with-sig-list
list-options show-notations

```

These options make gpg show more information about the subkeys,
information we are going to set and use.


## Creating the master key

We start by assigning the directory in the detachable drive to
``GNUPGHOME``:

```
$ {b}export GNUPGHOME="/media/cryptflash/dotgpg"{/b}
```

We create the directory and copy our ``~/.gnupg/gnupg.conf`` to it:

```
$ {b}mkdir -p "$GNUPGHOME"{/b}
$ {b}cp "$HOME/.gnupg/gpg.conf" "$GNUPGHOME/"{/b}
```

To keep the instructions below a bit more "copy-pasteable", let's use
an ``EMAIL`` variable and the following ``FULLNAME`` variable:

```
$ {b}FULLNAME=$(getent passwd "$USER" | sed -nE 's@^([^:]+:){4}([^,:]+).*@\2@p'){/b}
```

We can now generate the master key pair:

```
$ {b}gpg --quick-gen-key "$FULLNAME <$EMAIL>" default sign 0{/b}
gpg: keybox 'pubring.kbx' created
We need to generate a lot of random bytes. It is a good idea to perform
some other action (type on the keyboard, move the mouse, utilize the
disks) during the prime generation; this gives the random number
generator a better chance to gain enough entropy.
Enter passphrase: {b}mypassphrase{/b}
gpg: trustdb.gpg: trustdb created
gpg: directory 'openpgp-revocs.d' created
gpg: revocation certificate stored as 'openpgp-revocs.d/D6F57B3CE55D0CE481525268A59EF4105E6E41F0.rev'
public and secret key created and signed.
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                            Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]

```

That command asks you for a password, and then creates the master key
pair with default options and no expiration date. For more details on
why a master key expiration date is irrelevant in our scenario, read
[this](https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/).

Keep in mind that gpg's interface is... weird. It has its own REPL
that you can use if you want. In this article we are always invoking
it from the shell and passing the commands as arguments.

We'll use `gpg -k` to lists the public keys and their states quite
often as we create/edit keys in this article, starting now:

```
$ {b}gpg -k{/b}
gpg: checking the trustdb
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u
pubring.kbx
----------------------------
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]

```

The ``A59EF4105E6E41F0`` above is the generated ID of our master key and
we'll need it every time want to use the key. We might as well store
it in a variable:

```
$ {b}masterkeyid=A59EF4105E6E41F0{/b}
```


## Adding a secondary ID and email

If you have a secondary email, you should add another User ID:

```
$ {b}gpg --quick-add-uid "$FULLNAME <$EMAIL>" "$FULLNAME <llpenz@gmail.com>"{/b}
Enter passphrase: {b}mypassphrase{/b}

```

Notice that, by default, the last user ID becomes the primary and it's
trust status is `unknown`:

```
$ {b}gpg -k{/b}
pubring.kbx
----------------------------
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ unknown] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]

```

To make the old user ID the primary, select it with `uid 1` and invoke
`primary`; then run `gpg --check-trustdb` to update the trust status
of the new user ID:
```
$ {b}gpg --edit-key "$masterkeyid" uid 2 primary save{/b}
gpg (GnuPG) 2.4.7; Copyright (C) 2024 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Secret key is available.
sec  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1). Leandro Lisboa Penz <llpenz@gmail.com>
[ultimate] (2)  Leandro Lisboa Penz <lpenz@lpenz.org>
sec  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1). Leandro Lisboa Penz <llpenz@gmail.com>
[ultimate] (2)  Leandro Lisboa Penz <lpenz@lpenz.org>
sec  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1). Leandro Lisboa Penz <llpenz@gmail.com>
[ultimate] (2)* Leandro Lisboa Penz <lpenz@lpenz.org>
sec  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ unknown] (1)  Leandro Lisboa Penz <llpenz@gmail.com>
[ultimate] (2)* Leandro Lisboa Penz <lpenz@lpenz.org>
$ {b}gpg --check-trustdb{/b}
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u

```

These last 2 lines are probably familiar: sometimes, `gpg -k` updates
the trusted database, but not always.

We now have the 2 user IDs in our master key ``A59EF4105E6E41F0``, in
the correct order:

```
$ {b}gpg -k{/b}
pubring.kbx
----------------------------
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]

```

To see the corresponding private keys, use `gpg -K` (capital):

```
$ {b}gpg -K{/b}
pubring.kbx
----------------------------
sec   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]

```

The notable difference above is the *sec* keyword describing the
secret part of our master key pair.


## Importing the master public key in the host

We can now export the **public** part of the master key pair, and
import it in the host system. That allows us to check the signatures
of our subkeys and give them trust.

```
$ {b}gpg --armor --output /tmp/master.pub.gpg --export "$masterkeyid"{/b}

```

We don't actually need access to the master key flash drive after
exporting its public part. We can then reset `GNUPGHOME`.  That makes
`gpg` use the default database, to where we import the file:

```
$ {b}unset GNUPGHOME{/b}
$ {b}gpg --import /tmp/master.pub.gpg{/b}
gpg: keybox 'pubring.kbx' created
gpg: trustdb.gpg: trustdb created
gpg: key A59EF4105E6E41F0: public key "Leandro Lisboa Penz <lpenz@lpenz.org>" imported
gpg: Total number processed: 1
gpg:               imported: 1

```

And trust it:

```
$ {b}gpg --edit-key "$masterkeyid" trust save{/b}
gpg (GnuPG) 2.4.7; Copyright (C) 2024 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
pub  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: unknown       validity: unknown
[ unknown] (1). Leandro Lisboa Penz <lpenz@lpenz.org>
[ unknown] (2)  Leandro Lisboa Penz <llpenz@gmail.com>
pub  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: unknown       validity: unknown
[ unknown] (1). Leandro Lisboa Penz <lpenz@lpenz.org>
[ unknown] (2)  Leandro Lisboa Penz <llpenz@gmail.com>
Please decide how far you trust this user to correctly verify other users' keys
(by looking at passports, checking fingerprints from different sources, etc.)
  1 = I don't know or won't say
  2 = I do NOT trust
  3 = I trust marginally
  4 = I trust fully
  5 = I trust ultimately
  m = back to the main menu
Your decision? {b}5{/b}
Do you really want to set this key to ultimate trust? (y/N) {b}y{/b}
pub  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: unknown
[ unknown] (1). Leandro Lisboa Penz <lpenz@lpenz.org>
[ unknown] (2)  Leandro Lisboa Penz <llpenz@gmail.com>
Please note that the shown key validity is not necessarily correct
unless you restart the program.
Key not changed so no update needed.

```

Result:

```
$ {b}gpg -k{/b}
gpg: checking the trustdb
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u
pubring.kbx
----------------------------
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]

```

As we have imported only the public part of the master key pair,
`gpg -K` won't show us anything:

```
$ {b}gpg -K{/b}

```

And this is the setup. After it is done, we can do the following to
get the flash drive unmounted:

```
$ {b}pumount cryptflash{/b}
```

We can now remove the flash drive. Keep it safe.


# Adding a subkey

The single purpose of the master key is the generation of subkeys -
one for each combination of host and service, in a matrix-like
fashion. That allows us to track down all services affected by a
vulnerable host, and act accordingly.

First, we insert the master key flash drive, mount it and set
GNUPGHOME:

```
$ {b}cd /dev/disk/by-label{/b}
$ {b}pmount cryptflash{/b}
Enter passphrase for cryptflash: {b}mypassphrase{/b}
$ {b}export GNUPGHOME="/media/cryptflash/dotgpg"{/b}
```


## Creating the subkey

Before adding the subkey, we have to decide what we are going to use
it for so that we can put this information inside a *notation*.
Notations are key-value tags assigned to a signature, where the key
has the format `id@domain`, and *domain* acts as a namespace - more
information in [RFC4880]. We can also use a second notation to
identify the host where the key is installed. I'm actually using
`host@lpenz.org` for the hosts and `service@lpenz.org` for the
services.

So, to add a subkey for *github* that is installed in the host
*darkstar*:

```
$ {b}gpg --cert-notation host@lpenz.org=darkstar --cert-notation service@lpenz.org=github --edit-key "$masterkeyid" addkey save{/b}
gpg (GnuPG) 2.4.7; Copyright (C) 2024 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Secret key is available.
sec  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
[ultimate] (1). Leandro Lisboa Penz <lpenz@lpenz.org>
[ultimate] (2)  Leandro Lisboa Penz <llpenz@gmail.com>
Please select what kind of key you want:
   (3) DSA (sign only)
   (4) RSA (sign only)
   (5) Elgamal (encrypt only)
   (6) RSA (encrypt only)
  (10) ECC (sign only)
  (12) ECC (encrypt only)
  (14) Existing key from card
Your selection? {b}4{/b}
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (3072) {b}{/b}
Requested keysize is 3072 bits
Please specify how long the key should be valid.
         0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0) {b}{/b}
Key does not expire at all
Is this correct? (y/N) {b}y{/b}
Really create? (y/N) {b}y{/b}
Enter passphrase: {b}mypassphrase{/b}
We need to generate a lot of random bytes. It is a good idea to perform
some other action (type on the keyboard, move the mouse, utilize the
disks) during the prime generation; this gives the random number
generator a better chance to gain enough entropy.
sec  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
ssb  rsa3072/677DAE600EF4AABC
     created: 2026-08-04  expires: never       usage: S
[ultimate] (1). Leandro Lisboa Penz <lpenz@lpenz.org>
[ultimate] (2)  Leandro Lisboa Penz <llpenz@gmail.com>

```

## Importing the new subkey in the target system

After creating the new subkey in the master key flash drive, we have
to export the pair and then import it in the host where it will be
used.

```
$ {b}gpg --armor --output /tmp/newkey.sec.gpg --export-secret-subkey 677DAE600EF4AABC!{/b}
Enter passphrase: {b}mypassphrase{/b}

```

After exporting the file, we unset `GNUPGHOME` and import the file
into the default database:

```
$ {b}unset GNUPGHOME{/b}
$ {b}gpg --import /tmp/newkey.sec.gpg{/b}
gpg: key A59EF4105E6E41F0: "Leandro Lisboa Penz <lpenz@lpenz.org>" 1 new signature
gpg: key A59EF4105E6E41F0: "Leandro Lisboa Penz <lpenz@lpenz.org>" 1 new subkey
Enter passphrase: {b}mypassphrase{/b}
gpg: To migrate 'secring.gpg', with each smartcard, run: gpg --card-status
gpg: key A59EF4105E6E41F0: secret key imported
gpg: Total number processed: 1
gpg:            new subkeys: 1
gpg:         new signatures: 1
gpg:       secret keys read: 1
gpg:   secret keys imported: 1
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u

```

Results:

```
$ {b}gpg -k{/b}
pubring.kbx
----------------------------
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
sub   rsa3072/677DAE600EF4AABC 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github

$ {b}gpg -K{/b}
pubring.kbx
----------------------------
sec#  ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
ssb   rsa3072/677DAE600EF4AABC 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github

```

We can check that we can't add a new subkey without using `cryptflash`
as `GNUPGHOME`:

```
$ {b}gpg --edit-key "$masterkeyid" addkey save{/b}
gpg (GnuPG) 2.4.7; Copyright (C) 2024 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Secret subkeys are available.
pub  ed25519/A59EF4105E6E41F0
     created: 2026-08-04  expires: never       usage: SC
     trust: ultimate      validity: ultimate
ssb  rsa3072/677DAE600EF4AABC
     created: 2026-08-04  expires: never       usage: S
[ultimate] (1). Leandro Lisboa Penz <lpenz@lpenz.org>
[ultimate] (2)  Leandro Lisboa Penz <llpenz@gmail.com>
Need the secret key to do this.
Key not changed so no update needed.

```

We can also test that the signature still works with by signing a
"test" message:

```
$ {b}echo "test" | gpg --clearsign{/b}
-----BEGIN PGP SIGNED MESSAGE-----
Hash: SHA512
test
Enter passphrase: {b}mypassphrase{/b}
-----BEGIN PGP SIGNATURE-----
iQGzBAEBCgAdFiEE2rPXvRJUZkXSYUZiZ32uYA70qrwFAmpyMX0ACgkQZ32uYA70
qrzJ3gwAr5aoWO+iURiWSHePbjOFh2nPfVE2J6C5ILICevNIWnMauRFnUXJj8nWK
z/zPMfnD30BoNSSmhMNtVpjgYZv9a48vjBGMCcdf1QuXnVtv9gsjJLdbswue52ZD
NdCcernnJ+4AHhuBAPE0AT1mIxvmomt725ZtwUuxLYU+kp4NkJt24QSv++R4P8mQ
/d8fRfKIKhEQzgbOfbbIXzozswcocyfQO9KAbuEjU8riP5DBeX8La4zw9g8t9tzY
SVuL4vhRRC1FapMdq7FvRQ+HvsKBiOvCDZti9JyFCxqifjx7iUZI6gBiZG7s1Tm4
2BPfD90aLLt5qs6tlGsLF4iqFGxEz5MLPHTrUcBWM3hZfkg5Ad378l+TQx08mHX/
vz+IEL0s8fkgZMQasybAGyU/3M8CsS6D/TbnjApE+rHbJHa7ytfBv/E6eLHRePIz
2il/6DjrjYyiGCYocNXIhKyH7dvxINi/jFr/WRuakjxxAxx1g+5UqLM/Cr7EChEP
VR7FztP0
=s0VL
-----END PGP SIGNATURE-----

```


# Using the keys


## Configuring git

To configure your signing key in git:

```
$ {b}git config --global user.signingkey 677DAE600EF4AABC{/b}
```

To sign a commit, use `-S`:

```
$ {b}git commit -S -m 'Add a commit message here'{/b}
[main (root-commit) 725e15f] Add a commit message here
 1 file changed, 1 insertion(+)
 create mode 100644 test.txt

```

And we can see the signature with `--show-signature`:

```
$ {b}git log -1 --show-signature{/b}
commit 725e15fe7059c8ccc148138736f5b8215f52ffb3 (HEAD -> main)
gpg: Signature made Tue Aug  4 18:37:50 2026 UTC
gpg:                using RSA key DAB3D7BD12546645D2614662677DAE600EF4AABC
gpg: Good signature from "Leandro Lisboa Penz <lpenz@lpenz.org>" [ultimate]
gpg:                 aka "Leandro Lisboa Penz <llpenz@gmail.com>" [ultimate]
Primary key fingerprint: D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
     Subkey fingerprint: DAB3 D7BD 1254 6645 D261  4662 677D AE60 0EF4 AABC
Author: Leandro Lisboa Penz <lpenz@lpenz.org>
Date:   Tue Aug 4 18:37:50 2026 +0000
    Add a commit message here

```

We can also configure git to sign all commits by default:

```
$ {b}git config --global commit.gpgsign true{/b}
```


## Setting up key validation in service providers

One need the public key to be able to validate the corresponding
private key signature. To get that in a format that can be configured
in most services, we use:

```
$ {b}gpg --armor --export 677DAE600EF4AABC{/b}
-----BEGIN PGP PUBLIC KEY BLOCK-----
mDMEanIxbBYJKwYBBAHaRw8BAQdAgkpd44XlQlodAaapky1Fma28e6AjkKVzWPuL
oNGjVIq0JkxlYW5kcm8gTGlzYm9hIFBlbnogPGxscGVuekBnbWFpbC5jb20+iJAE
ExYKADgWIQTW9Xs85V0M5IFSUmilnvQQXm5B8AUCanIxbgIbAwULCQgHAgYVCgkI
CwIEFgIDAQIeAQIXgAAKCRClnvQQXm5B8DsaAQDaq9txyWpLVl3TPw9CD8fy9qtD
UJSIhJyD8KcMTuz6oAD/chgQSs73orS0n6e83hC7bZ7LpDchVD430Xw8Ey0qugW0
JUxlYW5kcm8gTGlzYm9hIFBlbnogPGxwZW56QGxwZW56Lm9yZz6IkwQTFgoAOwIb
AwULCQgHAgYVCgkICwIEFgIDAQIeAQIXgBYhBNb1ezzlXQzkgVJSaKWe9BBebkHw
BQJqcjFuAhkBAAoJEKWe9BBebkHw3coA/1qk+zq+Z7/6VEsoL8kwhIyZfDiSvf7L
wEa6QR+AOj3YAP9JDmGjl/D7H+hV3rFEwIW/eqhpM7F7Fn6N30mGEp62ArkBjQRq
cjFvAQwA3AxwtgLp/OJYMyRsdca80+A5ghjxOYCOC4EOuOghxKEQq5JMUmbFDdHK
qjqhnzxbyLnIIYI81sEXEGGFk4Ab3vaJE4QAdVUai5jqSeBJyOA+OXHfPBRKXgAz
bUeQsUxkd9Ti1w54of2olj+Q0WwnJEYkubyxdK30CxwnDGAheRo0WQauY5UPHsyj
IEW2iwTQfXYeLJGg4XQiesUjF9Klm3DhKwCSR2P0DTloF8UKfrL550Aj+AZXRwho
3eg4H6RReboShAtGEQeffkA+aaYfZvqKE0qV02dYjujD520eqbnuALjA+XaMIWnU
MKbAGyyWgkR/UDr+qMAVKAz5EPhbqRAyU0z9X4ftryjHfK15O2CJ0zEw/XDZHVKA
rDZGzId9D6rUBqifW6AhL52BsQ9JWfZEuLk0w/r6PXac9icFwR4uGmwd4wqHEsLQ
/p9MpF0dd+KL8A9oxkyyQJc8VFOHfU4CLa3+wadr8x5Dr6ePVREe+yh/PVT64ymH
DS916oLvABEBAAGJAm8EGBYKAGEWIQTW9Xs85V0M5IFSUmilnvQQXm5B8AUCanIx
byAUgAAAAAARAAZzZXJ2aWNlQGxwZW56Lm9yZ2dpdGh1Yh8UgAAAAAAOAAhob3N0
QGxwZW56Lm9yZ2RhcmtzdGFyAhsCAcAJEKWe9BBebkHwwPQgBBkBCgAdFiEE2rPX
vRJUZkXSYUZiZ32uYA70qrwFAmpyMW8ACgkQZ32uYA70qrwRcwv/XUxNvi88jN9f
8/JaMn4BJ09dbigMnhJ69K/mD2MvlxtO4Wv9Cox+kDtj7IVGC0wz+YSlv1IZgGv+
1wFsncY/jmsBAhGhB3zh0NFl6/NFfyY/fWeAly9YLPwp1P5mWFIZF0GS5BgUg4PN
ajPzDpZuAy9U7VjJf/GYHR7OHTOngMcpbgJDsL/GAbyJuNE4EPr65+pEcKagxg9L
9cGkcUsbPvASRspBRYD8zqbvrLNA7t0QFjBPL5p85IborVlwRE7PMBF2eWWFT9dj
VHYhUrO/RrSQ8R95KxzHKVHKb22GS5O/Sl1k3AUeDylIYwpXU9iRVjpwWhK9HlyA
/TJjI2/ipqQfykSsuuMzKnGZmiLN5k+nI+xDrkOYljzsWcXr6xO9hKe93ntBhx5q
LBmFI/oyBTy1clooAtrZjJtPqrCESOTs+Lz4FNOzlZaA2q81cTZXfDPazlEjSHdc
lnr91IrpzBEsgDsbN864KqYoh1Bdk+Or8PM0ag2rn5WWDlLim5v30o8BAIaWz83a
hDCPbtEBuyOsiBBrdkxI701Y0No019vJaMxCAQCa5TJegdEAiOqXgVo7tmSSaraI
yWPEEaK6Br20/38+DQ==
=BMBb
-----END PGP PUBLIC KEY BLOCK-----

```

We then copy that to the clipboard, and paste in the settings page of
the services. Here we have the instructions for some git hosting
services:

- GitHub: <https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account>
- GitLab: <https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account>
- Bitbucket: <https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add>


# Addendum: key list details

To list the public keys:

```
$ {b}gpg -k{/b}
pubring.kbx
----------------------------
pub   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
sub   rsa3072/677DAE600EF4AABC 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github
sub   rsa3072/6F5FCC4AF89A4550 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=github
sub   rsa3072/D13A690485C01EAA 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=email
sub   rsa3072/648066CFDB9EF34E 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=github
sub   rsa3072/40E1795F01E156A4 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=email

```

To list the private keys:

```
$ {b}gpg -K{/b}
pubring.kbx
----------------------------
sec   ed25519/A59EF4105E6E41F0 2026-08-04 [SC]
      Key fingerprint = D6F5 7B3C E55D 0CE4 8152  5268 A59E F410 5E6E 41F0
uid                 [ultimate] Leandro Lisboa Penz <lpenz@lpenz.org>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
uid                 [ultimate] Leandro Lisboa Penz <llpenz@gmail.com>
sig 3        A59EF4105E6E41F0 2026-08-04  [self-signature]
ssb   rsa3072/677DAE600EF4AABC 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=darkstar
   Signature notation: service@lpenz.org=github
ssb   rsa3072/6F5FCC4AF89A4550 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=github
ssb   rsa3072/D13A690485C01EAA 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=redplanet
   Signature notation: service@lpenz.org=email
ssb   rsa3072/648066CFDB9EF34E 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=github
ssb   rsa3072/40E1795F01E156A4 2026-08-04 [S]
sig      N   A59EF4105E6E41F0 2026-08-04  [self-signature]
   Signature notation: host@lpenz.org=brightmoon
   Signature notation: service@lpenz.org=email

```

In the output above:

- *pub* identifies a public primary key entry.
- *sub* identifies a public subkey entry.
- *sec* identifies a private key entry, and the *#* marks the key as
  absent (we are not looking at the USB drive).
- *ssb* identifies a private subkey entry.
- *uid* and *sig* show information about the current key entry.
- *N* means the key has notations, which are shown below.


# References

- Why expiration dates are irrelevant:
  <https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/>
- Ana Guerrero López tutorial that uses subkeys:
  <http://ekaia.org/blog/2009/05/10/creating-new-gpgkey/>
- There's also a Debian page about subkeys:
  <https://wiki.debian.org/Subkeys>
- Generating a revocation certificate with gpg:
  <https://debian-administration.org/article/450/Generating_a_revocation_certificate_with_gpg>
- Getting information from an armored gpg public key file:
  <https://superuser.com/questions/173417/getting-information-from-an-armored-gpg-public-key-file>
- Secure yourself, Part 1: Air-gapped computer, GPG and smartcards:
  <https://viccuad.me/blog/Revisited-secure-yourself-part-1-airgapped-computer-and-gpg-smartcards>
- [RFC4880]
- Details about listing format: <https://github.com/gpg/gnupg/blob/master/doc/DETAILS>
- Signatures in git:
  <https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work>
- Adding the public GPG key to providers:
  - GitHub: <https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account>
  - GitLab: <https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account>
  - Bitbucket: <https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add>


[RFC4880]: https://tools.ietf.org/html/rfc4880#section-5.2.3.16
