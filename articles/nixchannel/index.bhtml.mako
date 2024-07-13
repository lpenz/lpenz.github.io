<div class="body" id="body">
<p>
TL;DR: full example at <a href="https://github.com/lpenz/nixpkgs-lpenz/">https://github.com/lpenz/nixpkgs-lpenz/</a>
</p>
<p>
<a href="https://nixos.org">Nix</a> is a packet manager for Unix-like systems that
can be effectively used as an overlay distribution.
</p>
<p>
These are my notes on how to create a static nix channel with a binary
cache that can be used to distribute compiled packages - and how to do
it using <a href="https://travis-ci.com">travis-ci</a> and
<a href="https://pages.github.com/">github pages</a>. This assumes that we already
have the derivations that we want to distribute.
</p>
<p>
You should also check out <a href="https://cachix.org/">Cachix</a>, as an
alternative to what is described here.
</p>

<section>
<h1>Generating key pair</h1>

<p>
The first step in creating a repository is generating a key pair:
</p>

<pre>
nix-store --generate-binary-cache-key lpenz.org cache-priv-key.pem cache-pub-key.pem
</pre>

<p>
That creates the single-line files <code>cache-priv-key.pem</code> and
<code>cache-pub-key.pem</code>. The first has the private key, which we will
use to do the packaging process in travis-ci (store it in a safe
place). The second file has the public key that we will use to
authenticate the packages in the clients.
</p>

</section>
<section>
<h1>Setting up the repository in github and travis-ci</h1>

<p>
This article is not going into the details of how to create a repository and populate it with nix derivations. Here are some references on that:
</p>

<ul>
<li><a href="https://nixos.org/nixos/nix-pills/generic-builders.html">https://nixos.org/nixos/nix-pills/generic-builders.html</a>
</li>
<li><a href="https://nixos.org/nixpkgs/manual/#chap-stdenv">https://nixos.org/nixpkgs/manual/#chap-stdenv</a>
</li>
</ul>

<p>
To set up github pages, go to the <em>Settings</em> page of the github
repsitory with the derivations, <em>GitHub Pages</em> and set <em>Source</em> to
the <em>gh-pages branch</em>.
</p>
<p>
To set up travis-ci, create a <em>NIX_CACHE_PRIV_KEY</em> environment variable in
the settings
(<a href="https://docs.travis-ci.com/user/environment-variables/">details</a>),
with the contents of <code>cache-priv-key.pem</code>. Keep in mind the best
practices at
<a href="https://docs.travis-ci.com/user/best-practices-security#recommendations-on-how-to-avoid-leaking-secrets-to-build-logs">Recommendations on how to avoid leaking secrets to build logs</a>
to avoid problems.
</p>

<section>
<h2>Creating the channel</h2>

<p>
A channel is simply a <code>nixexprs.tar.xz</code> with all nix derivation
files, and a <code>binary-cache-url</code> file that, as the name implies,
points to the binary cache URL.
</p>
<p>
Let's assume that we are creating the channel in the <code>_output</code>
directory, and that we will put the cache files in
<code>_output/cache</code>. To create the <code>nixexprs.tar.xz</code> file:
</p>

<pre>
tar -cJf _output/nixexprs.tar.xz ./*.nix $backslash$
    --transform "s,^,${"$"}{PWD##*/}/," $backslash$
    --owner=0 --group=0 --mtime="1970-01-01 00:00:00 UTC"
</pre>

<p>
Creating the <code>binary-cache-url</code> file is simpler:
</p>

<pre>
echo '&lt;URL of cache&gt;' &gt; _output/binary-cache-url
</pre>

<p>
Nix also tries to retrieve the channel URL by itself - create a
<code>index.html</code> file to handle that:
</p>

<pre>
touch _output/index.html
</pre>

</section>
<section>
<h2>Creating the binary cache</h2>

<p>
To create the binary cache, start by building the packages with:
</p>

<pre>
nix-build
</pre>

<p>
Then sign the results:
</p>

<pre>
export NIX_SECRET_KEY_FILE="$PWD/nix-cache-priv-key.pem"
echo "$NIX_CACHE_PRIV_KEY" &gt; "$NIX_SECRET_KEY_FILE"
nix store sign -k "$NIX_SECRET_KEY_FILE"
</pre>

<p>
And finally, copy the results from the store to the cache directory:
</p>

<pre>
nix copy --to "file:///$PWD/_output/cache"
</pre>

</section>
<section>
<h2>.travis.yml deploy section</h2>

<p>
Detailed instructions on how to set this up are at
<a href="https://docs.travis-ci.com/user/deployment/pages/">GitHub Pages Deployment</a>.
We essentially have to set up a GITHUB_TOKEN.
</p>
<p>
The deploy section of <code>.travis.yml</code> ends up looking like the following:
</p>

<pre>
deploy:
  provider: pages
  skip-cleanup: true
  github-token: "$GITHUB_TOKEN"
  local_dir: _output
  target-branch: gh-pages
  on:
    branch: master
</pre>

</section>
</section>
<section>
<h1>Using the channel</h1>

<p>
To use the channel, add the channel URL to nix:
</p>

<pre>
nix-channel --add &lt;URL of channel&gt;
nix-channel --update
</pre>

<p>
That allows us to build packages from the channel, but it doesn't
make nix use the binary cache. To enable the cache, we have to set up
the channel as a <em>substituter</em>. To do that, put the following in
<code>~/.config/nix/nix.conf</code>, creating the file it if it doesn't exist:
</p>

<pre>
substituters = https://cache.nixos.org &lt;URL of cache&gt;
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= &lt;cache-pub-key.pem contents&gt;
</pre>

<p>
Note that we have to also add the default, as that line is an absolute
configuration.
</p>

</section>
<section>
<h1>Conclusions</h1>

<p>
Nix is very versatile, and one way to use it is an overlay
distribution, by installing <a href="https://github.com/NixOS/nixpkgs">nixpkgs</a>
over an existing non-NixOS linux installation. On that setup, a
private nix channel can be very useful to develop and deploy software
consistently without being pegged to a particular gloal distribution
or environment - and the binary cache allows us to do that without
even having to recompile from source. That's how I've been using it,
and I'm quite happy with the results.
</p>
</section>
</div>
