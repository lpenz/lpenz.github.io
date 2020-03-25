---
title: Using GPG and signing git commits
subtitle: Comment those subkeys
date: 2022-05-22
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
$ {b}cd /dev/disk/by-label{/b}
$ {b}pmount cryptflash{/b}
Enter passphrase for cryptflash: {b}mypassphrase{/b}
```

Note that `pmount` doesn't need any configuration to do that - it
detects luks and the filesystem, and mounts that partition in
`/media/cryptflash`. We assume this path is being used below.


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
${genkey}
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
${gpglist1}
```

The ``${masteruuid}`` above is the generated ID of our master key and
we'll need it every time want to use the key. We might as well store
it in a variable:

```
$ {b}masterkeyid=${masteruuid}{/b}
```


${"##"} Adding a secondary ID and email

If you have a secondary email, you should add another User ID:

```
${adduid}
```

Notice that, by default, the last user ID becomes the primary and it's
trust status is `unknown`:

```
${gpglist2}
```

To make the old user ID the primary, select it with `uid 1` and invoke
`primary`; then run `gpg --check-trustdb` to update the trust status
of the new user ID:
```
${uidprimary}
```

These last 2 lines are probably familiar: sometimes, `gpg -k` updates
the trusted database, but not always.

We now have the 2 user IDs in our master key ``${masteruuid}``, in
the correct order:

```
${gpglist3}
```

To see the corresponding private keys, use `gpg -K` (capital):

```
${gpgprivlist3}
```

The notable difference above is the *sec* keyword describing the
secret part of our master key pair.


${"##"} Importing the master public key in the host

We can now export the **public** part of the master key pair, and
import it in the host system. That allows us to check the signatures
of our subkeys and give them trust.

```
${masterkeyexport}
```

We don't actually need access to the master key flash drive after
exporting its public part. We can then reset `GNUPGHOME`.  That makes
`gpg` use the default database, to where we import the file:

```
$ {b}unset GNUPGHOME{/b}
${masterkeyimport}
```

And trust it:

```
${masterkeytrust}
```

Result:

```
${gpglistmasterimport}
```

As we have imported only the public part of the master key pair,
`gpg -K` won't show us anything:

```
${gpgprivlistmasterimport}
```

And this is the setup. After it is done, we can do the following to
get the flash drive unmounted:

```
$ {b}pumount cryptflash{/b}
```

We can now remove the flash drive. Keep it safe.


${"#"} Adding a subkey

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


${"##"} Creating the subkey

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
${subkeyadd}
```

${"##"} Importing the new subkey in the target system

After creating the new subkey in the master key flash drive, we have
to export the pair and then import it in the host where it will be
used.

```
${subkeyexport}
```

After exporting the file, we unset `GNUPGHOME` and import the file
into the default database:

```
$ {b}unset GNUPGHOME{/b}
${subkeyimport}
```

Results:

```
${subkeylist}
${subkeyprivlist}
```

We can check that we can't add a new subkey without using `cryptflash`
as `GNUPGHOME`:

```
${subkeyaddfail}
```

We can also test that the signature still works with by signing a
"test" message:

```
${gpgsigntest}
```


${"#"} Using the keys


${"##"} Configuring git

To configure your signing key in git:

```
$ {b}git config --global user.signingkey ${subkeyuuid}{/b}
```

To sign a commit, use `-S`:

```
${gitcommit}
```

And we can see the signature with `--show-signature`:

```
${gitlogsign}
```

We can also configure git to sign all commits by default:

```
$ {b}git config --global commit.gpgsign true{/b}
```


${"##"} Setting up key validation in service providers

One need the public key to be able to validate the corresponding
private key signature. To get that in a format that can be configured
in most services, we use:

```
${gpgprintkey}
```

We then copy that to the clipboard, and paste in the settings page of
the services. Here we have the instructions for some git hosting
services:

- GitHub: <https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account>
- GitLab: <https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account>
- Bitbucket: <https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add>


${"#"} Addendum: key list details

To list the public keys:

```
${listpub}
```

To list the private keys:

```
${listpriv}
```

In the output above:

- *pub* identifies a public primary key entry.
- *sub* identifies a public subkey entry.
- *sec* identifies a private key entry, and the *#* marks the key as
  absent (we are not looking at the USB drive).
- *ssb* identifies a private subkey entry.
- *uid* and *sig* show information about the current key entry.
- *N* means the key has notations, which are shown below.


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
- [RFC4880]
- Details about listing format: <https://github.com/gpg/gnupg/blob/master/doc/DETAILS>
- Signatures in git:
  <https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work>
- Adding the public GPG key to providers:
  - GitHub: <https://docs.github.com/en/authentication/managing-commit-signature-verification/adding-a-new-gpg-key-to-your-github-account>
  - GitLab: <https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/#add-a-gpg-key-to-your-account>
  - Bitbucket: <https://confluence.atlassian.com/bitbucketserver/using-gpg-keys-913477014.html#UsingGPGkeys-add>


[RFC4880]: https://tools.ietf.org/html/rfc4880#section-5.2.3.16
