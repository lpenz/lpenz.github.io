<div class="body" id="body">
<p>
This page is basically my blog. My interests wander mostly around math,
statistics and software.
</p>

<section>
<h1>Posts</h1>

<dl>
<dt>2023-07-30</dt><dd>
New article: <a href="articles/rust-snippets/index.html">Rust snippets</a>.
</dd>
</dl>

<dl>
<dt>2023-06-18</dt><dd>
<a href="https://en.wikipedia.org/wiki/Debian_version_history">Debian Bookworm</a> was just released. Time to update the environment (Dockerfile) that builds this website.
</dd>
</dl>

<dl>
<dt>2022-05-22</dt><dd>
While looking at how to set up <em>verified</em> commits in <a href="https://github.com">github</a>, I ended up with a scheme using a vaulted private master key and multiple private subkeys, with notations documenting their usage. I've written down some notes in <a href="articles/gpg/index.html">Using GPG and signing git commits</a>.
</dd>
</dl>

<dl>
<dt>2021-07-11</dt><dd>
I've been standardizing my github repositories and found some common patterns, now documented in <a href="articles/github-project-struct/index.html">github project structure best practices</a>. This is probably the one article that I'll be constantly updating as things change.
</dd>
</dl>

<dl>
<dt>2021-06-07</dt><dd>
<a href="articles/cryptusb/index.html">Creating a pmount-compatible encrypted USB drive</a> has notes specific on how to use LUKS to encrypt removable media.
</dd>
</dl>

<dl>
<dt>2020-03-22</dt><dd>
While looking up (once again) how to use LUKS, I figured I had some notes laying around. Finished them up in <a href="articles/luksfile/index.html">Creating an encrypted directory-in-a-file</a>.
</dd>
</dl>

<dl>
<dt>2019-07-07</dt><dd>
I've written down some notes on how one can go about <a href="articles/nixchannel/index.html">Creating a static nix channel</a> that can be deployed with <a href="https://travis-ci.com">travis-ci</a> to <a href="https://pages.github.com/">github pages</a>. I've created one for myself <a href="https://github.com/lpenz/nixpkgs-lpenz">here</a>.
</dd>
</dl>

<dl>
<dt>2019-06-25</dt><dd>
Updated the <a href="articles/liveusb/index.html">Debian live USB</a> article with Debian Stretch kernel version numbers, and with instructions on how to test with qemu.
</dd>
</dl>

<dl>
<dt>2019-01-08</dt><dd>
Wrote down some notes on <a href="articles/ansiblerpi/index.html">Provisioning a Raspberry Pi using ansible</a>. I'm currently using a hybrid Raspbian-Debian installation (Raspbianbian?)
</dd>
</dl>

<dl>
<dt>2018-05-16</dt><dd>
The Debian repository was removed from this domain, as I now keep all my packages in <a href="https://packagecloud.io/lpenz/lpenz">packagecloud</a>. I have also removed my GPG public key, as that was used only for Debian package signing, and I have lost the private key :)
</dd>
</dl>

<dl>
<dt>2018-04-05</dt><dd>
<code>git-buildpackage</code> commands changed, so the article <a href="articles/debgit/index.html">Debianization with git-buildpackage</a> had to be fixed and updated.
</dd>
</dl>

<dl>
<dt>2016-07-24</dt><dd>
The new subdomain <a href="http://cv.lpenz.org/">http://cv.lpenz.org/</a> now has my Curriculum Vitae, rendered from <a href="http://github.com/lpenz/cv">http://github.com/lpenz/cv</a> with continuous deployment.
</dd>
</dl>

<dl>
<dt>2015-10-04</dt><dd>
The <a href="articles/liveusb/index.html">Create a Debian bootable live USB</a> article shows my notes on how to create a rescue USB stick, or portable environment.
</dd>
</dl>

<dl>
<dt>2013-12-09</dt><dd>
The <a href="articles/bugprobhunt/index.html">Probabilistic bug hunting</a> article shows us how to deal with software (or hardware, for that matter) bugs that do not have a deterministic behaviour.
</dd>
</dl>

<dl>
<dt>2013-03-17</dt><dd>
I have built a new GPG key, and re-signed all my debian packages with it.
</dd>
</dl>

<dl>
<dt>2012-10-21</dt><dd>
I always use <em>watch</em> to monitor the output of Unix processes and wait for events. I sometimes wonder about the history of outputs. Enters <a href="https://github.com/lpenz/watchng">watchng</a>, a python script that shows the output of a process along with the date-time, but only when this output changes between two consecutive runs.
</dd>
</dl>

<dl>
<dt>2011-11-15</dt><dd>
<a href="http://www.google.com/sheets/about/">Google sheets</a> is a great tool for collaboration. It's a spreadsheet where a team can simultaneously edit cells online, in a clear and consistent fashion. To ease the use of the product of this collaboration in other contexts, you can now use <a href="https://github.com/lpenz/google-spreadsheet-csv">google-spreadsheet-csv</a> to download and upload the CSV version of a sheet.
</dd>
</dl>

<dl>
<dt>2011-06-23</dt><dd>
The last article in the "HD occupation series", <a href="articles/df0pred-3/index.html">Hard drive occupation prediction with R - Part 3</a>, explores how we can use historical data and Monte Carlo simulations to predict the range of possible values for HD occupation at any point in the future.
</dd>
</dl>

<dl>
<dt>2011-02-16</dt><dd>
<a href="http://www.afterthedeadline.com/">After the Deadline</a> is a English language checker used by <a href="https://wordpress.com/">Wordpress.com</a> among others. I have built a command-line client for it: the <a href="https://github.com/lpenz/atdtool">atdtool</a>.
</dd>
</dl>

<dl>
<dt>2011-02-02</dt><dd>
The <a href="articles/hedsl-sharedexpenses/index.html">Haskell eDSL Tutorial - Shared expenses</a> explores the concept of domain-specific-language in Haskell by tackling a practical problem: sharing trip expenses.
</dd>
</dl>

<dl>
<dt>2011-01-22</dt><dd>
The second article on the "HD occupation" series, <a href="articles/df0pred-2/index.html">Hard drive occupation prediction with R - Part 2</a>, shows us where the linear regression breaks (hint: real life), and uses Monte Carlo simulations to build a more robust method.
</dd>
</dl>

<dl>
<dt>2010-08-15</dt><dd>
The <a href="articles/df0pred-1/index.html">Hard drive occupation prediction with R</a> article shows how to predict future free partition occupation by using a simple linear regression.
</dd>
</dl>

<dl>
<dt>2010-04-11</dt><dd>
My notes on how to debianize a git repository now available online in the <a href="articles/debgit/index.html">Debianization with git-buildpackage</a> article
</dd>
</dl>

</section>
</div>
