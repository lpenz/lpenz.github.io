---
title: Creating and managing signature keys with GPG
subtitle: Comment those subkeys
date: 2020-03-22
...


${"#"} Introduction

github, linux packages and repositories, email... a cryptographic
digital signature can be used in several places to
provide *authenticity* - a reasonable proof that the artifact was
indeed generate by the author.

These are my notes on gpg signature management. gpg has changed a lot
since I last used it, and I've decided to shift to a new strategy -
might as well write it all down this time.

The notation in this article uses {b}bold{/b} for user input.


${"##"} Basics of public key infrastructure (PKI)

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



${"##"} The strategy

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



${"#"} Master key setup

The instruction in this section are the initial setup and master key
creation. It should be done only once.


${"##"} Using a flash drive

Part of our strategy involves keeping the master key secure. One way
of doing that is by keeping it in a flash drive that is physically
kept secure. We could create and work with the key locally and then
move it away, but we are instead going to work with it straight from
a flash drive.

The master key is already secured by a password, so there's no need to
encrypt the flash drive because of it. If you want to keep other
sensitive files there, though, you should encrypt it. You can take a
look at the
[Creating an encrypted directory-in-a-file]($cwd$/../luksfile/index.html)
article for basic instructions.

In my particular setup, I'm using a flash drive with a luks2-encrypted
partition labeled *cryptflash* that I mount with:

```
$ {b}mkdir -p cryptflash{/b}
$ {b}sudo unshare -m sudo -u "$USER" -i{/b}
$ {b}cryptmount cryptflash{/b}
Enter password for target "cryptflash":
e2fsck 1.45.5 (07-Jan-2020)
/dev/mapper/gpg: clean, 33/4096 files, 1896/16384 blocks
```

And umount by just exiting the namespace shell.

The examples below assume we are using the ``cryptflash`` directory.


${"##"} Configuring gpg

It's worth noting that the ages-old interface design of gpg doesn't
support this approach in an intuitive way with the default
configuration. The first thing we should do is add a couple of lines
to *~/.gnupg/gpg.conf*:

```
${ "articles/gpg/gpg.conf" | includefile }
```

These options make gpg show more information about the subkeys,
information we are going to set and use.


${"##"} Creating the master key

We start by assigning the directory in the detachable drive to
``GNUPGHOME``:

```
$ {b}export GNUPGHOME="$HOME/cryptflash/dotgpg"{/b}
```

We create the directory and copy our ``~/.gnupg/gnupg.conf`` to it:

```
$ {b}mkdir -p "$GNUPGHOME"{/b}
$ {b}cp "$HOME/.gnupg/gpg.conf" "$GNUPGHOME/"{/b}
```

We can now generate the master key pair:

```
${genkey}
```

That command asks you for a password, and then creates the master key
pair with default options and no expiration date. For more details on
why a master key expiration date is irrelvant in our scenario, read
[this](https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/).

Keep in mind that gpg's interface is... weird. It has its own REPL
that you can use if you want. In this article we are always invoking
it from the shell and passing the commands as arguments; we finish
with ``save`` because that's the command that commits the changes and
gets out of the REPL.

If you have a secondary email, you should add another User ID:

```
${adduid}
```

Notice that, by default, the last UID becomes the primary. If you want
to keep the first one as the primary, select it with `uid 1` and mark
it with `primary`:
```
${uidprimary}
```

And this is the setup. After it is done, you can just exit the the
shell to get the flash drive unmounted. Keep it safe.


${"#"} Adding a subkey

The single purpose of the master key is the generation of subkeys -
one for each combination of host and service, in a matrix-like
fashion. That allows us to track down all services affected by a
vulnerable host, and act accordingly.

First, we insert the master key flash drive, mount it in a private
namespace and make gpg use it.

```
$ {b}sudo unshare -m sudo -u "$USER" -i{/b}
$ {b}cryptmount cryptflash{/b}
Enter password for target "cryptflash":
e2fsck 1.45.5 (07-Jan-2020)
/dev/mapper/gpg: clean, 33/4096 files, 1896/16384 blocks
$ {b}export GNUPGHOME="$HOME/cryptflash/dotgpg"{/b}
```


${"##"} Creating the subkey

Before adding the subkey, we have to decide what we are going to use
it for so that we can put this information inside a *notation*.
Notations are key-value tags assigned to a key, where the key has the
format `id@domain`, where *domain* acts as a namespace - more
information in [RFC4880]. We can also use a second notation to
identify the host where the key is installed. I'm actually using
`host@lpenz.org` for the hosts and `service@lpenz.org` for the
services.

So, to add a subkey for *github* that is installed in the host
*darkstar*:

```
${subkeyadd}
```


${"##"} Exporting the new subkey

After creating the new subkey in the master key flash drive, we have
to export the pair and then import it in the host where it will be
used.

```
${subkeyexport}
```


${"##"} Importing the new subkey in the target system


```
${subkeyimport}
```


${"#"} Listing keys

To list the public keys:

```
${listpub}
```

To list the private keys:

```
${listpriv}
```


${"#"} References

- Why expiration dates are irrelevant:
  <https://security.stackexchange.com/questions/14718/does-openpgp-key-expiration-add-to-security/>
- Ana Guerrero López tutorial that uses subkeys:
  <http://ekaia.org/blog/2009/05/10/creating-new-gpgkey/>
- Generating a revocation certificate with gpg:
  <https://debian-administration.org/article/450/Generating_a_revocation_certificate_with_gpg>
- Getting information from an armored gpg public key file:
  <https://superuser.com/questions/173417/getting-information-from-an-armored-gpg-public-key-file>
- Secure yourself, Part 1: Air-gapped computer, GPG and smartcards:
  <https://viccuad.me/blog/Revisited-secure-yourself-part-1-airgapped-computer-and-gpg-smartcards>

- [RFC4880]: https://tools.ietf.org/html/rfc4880#section-5.2.3.16

## -*- mode: markdown -*-
