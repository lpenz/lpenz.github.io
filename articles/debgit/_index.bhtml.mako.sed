<div id="toc" style="margin-bottom: 2em;">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#prerequisites" id="toc-prerequisites"><span
class="toc-section-number">1</span> Prerequisites</a></li>
<li><a href="#initial-packaging-setup"
id="toc-initial-packaging-setup"><span
class="toc-section-number">2</span> Initial packaging setup</a></li>
<li><a href="#importing-the-sources"
id="toc-importing-the-sources"><span class="toc-section-number">3</span>
Importing the sources</a></li>
<li><a href="#creating-the-package" id="toc-creating-the-package"><span
class="toc-section-number">4</span> Creating the package</a></li>
<li><a href="#importing-further-versions"
id="toc-importing-further-versions"><span
class="toc-section-number">5</span> Importing further versions</a></li>
<li><a href="#final-remarks" id="toc-final-remarks"><span
class="toc-section-number">6</span> Final remarks</a></li>
</ul>
</div>
<p><strong>Updated 2018-04-03</strong>: <em>git-buildpackage</em>’s
commands have changed, so this article had to be fixed; I took the
opportunity to improve a few things as well.</p>
<p>After building some useful piece of software, one has to decide how
to best deploy it. In UNIX, the standard way to do that is by publishing
the source code in .tar.gz format and requiring users to compile it.</p>
<p>In Debian there is an alternative: using a .deb package. With a .deb
package, a single <code>dpkg -i #dollar#{PACKAGE}.deb</code> installs
the software.</p>
<p>This article explains how to create and support a .deb package for a
simple software maintained in git, by tracking the packaging scheme in a
specific branch on the same repository.</p>
<h1 data-number="1" id="prerequisites"><span
class="header-section-number">1</span> Prerequisites</h1>
<p>In order to ease the packaging and keep our package warning-free, it
should have in its main repository:</p>
<ul>
<li>An <code>AUTHORS</code> file with copyright information.</li>
<li>A manual: <code>#dollar#{PACKAGE}.1</code> or similar.</li>
<li>A <code>COPYING</code> file with GPL information or some other
license.</li>
<li>An appropriate build file for the package. For C/C++ programs, I
recommend using <code>cmake</code>; for python, <code>setup.py</code>,
etc.</li>
</ul>
<p>These items are not debian-specific and are useful for everyone.</p>
<h1 data-number="2" id="initial-packaging-setup"><span
class="header-section-number">2</span> Initial packaging setup</h1>
<p>The first step is creating the
<code>#dollar#{PACKAGE}_#dollar#{VERSION}.orig.tar.gz</code> file. You
can use git itself for that, by running the following commands in the
repository:</p>
<pre><code>PREFIX=#dollar#{PACKAGE}_#dollar#{VERSION}
git archive --format=tar --prefix=$PREFIX/ $VERSION | gzip -c &gt; ../$PREFIX.orig.tar.gz</code></pre>
<p>You can check the contents of the archive with <em>tar</em>. If there
are extraneous files in the archive, you can configure git-archove to
exclude them by creating a <code>.gitattributes</code> file; for
example:</p>
<pre><code>.gitignore      export-ignore
.gitattributes  export-ignore
.travis.yml     export-ignore</code></pre>
<p>The next step is to create the debian branches in the git repository:
on the debian-upstream branch, we store the upstream source, while the
debian-debian branch holds the debian package data. This separation
provides a cleaner revision history by separating the changes that
affect the software from the changes in the packaging.</p>
<p>In order to create these branches, we issue the following commands in
the git repository:</p>
<pre><code>git checkout --orphan debian-upstream
git rm --cached -r .
git clean -xfd
git commit --allow-empty -m &#39;Start of debian branches.&#39;
git checkout -b debian-debian</code></pre>
<p>That creates both branches as orphans, pointing to an empty root
commit.</p>
<p>We now use the
<code>../#dollar#{PACKAGE}_#dollar#{VERSION}.orig.tar.gz</code> file to
create the initial <code>debian</code> directory in the debian-debian
branch:</p>
<pre><code>dh_make -s -p #dollar#{PACKAGE}_#dollar#{VERSION}</code></pre>
<p>We can now customize the standard <code>debian</code> directory
created. You must edit the following files: <code>changelog</code>,
<code>control</code>, <code>copyright</code> and <code>rules</code>.
Besides those, the <code>compat</code> file must be present; the other
files can be safely removed.</p>
<p>After changing the files that <code>dh_make</code> created, you
should create a <code>debian/gbp.conf</code> with the following
contents:</p>
<pre><code>[DEFAULT]
upstream-branch=debian-upstream
debian-branch=debian-debian</code></pre>
<p>We can now commit the debian directory in the debian-debian
branch.</p>
<h1 data-number="3" id="importing-the-sources"><span
class="header-section-number">3</span> Importing the sources</h1>
<p>In the debian-debian branch:</p>
<pre><code>gbp import-orig --no-interactive ../#dollar#{PACKAGE}_#dollar#{VERSION}.orig.tar.gz</code></pre>
<p>That imports the original sources to the debian-upstream branch, and
merge it into the debian-debian branch.</p>
<h1 data-number="4" id="creating-the-package"><span
class="header-section-number">4</span> Creating the package</h1>
<p>To create the debian package:</p>
<pre><code>gbp buildpackage -us -uc --git-tag</code></pre>
<h1 data-number="5" id="importing-further-versions"><span
class="header-section-number">5</span> Importing further versions</h1>
<p>Create the new
<code>../#dollar#{PACKAGE}_#dollar#{VERSION}/.orig.tar.gz</code> and
then:</p>
<pre><code>gbp import-orig --no-interactive ../#dollar#{PACKAGE}_#dollar#{VERSION}.orig.tar.gz</code></pre>
<p>Edit the <code>debian/changelog</code> file (we can use
<code>dch -i -v $VERSION</code> for that), and create a new package:</p>
<pre><code>gbp buildpackage -us -uc --git-tag</code></pre>
<p>Yes, it’s that easy.</p>
<h1 data-number="6" id="final-remarks"><span
class="header-section-number">6</span> Final remarks</h1>
<p>After an initial expensive setup, package creation of further
versions is mostly painless, which is the whole point of
git-buildpackage and friends.</p>
<p>Besides this article, we should check the <code>debian</code> dir of
some already packaged software for reference. We can look at the <a
href="https://github.com/lpenz/execpermfix">execpermfix</a> repository
at <a href="https://github.com">github</a> when first trying to package
something.</p>
<p>Further information:</p>
<ul>
<li><a href="https://www.eyrie.org/~eagle/notes/debian/git.html"
class="uri">https://www.eyrie.org/~eagle/notes/debian/git.html</a></li>
<li><a
href="http://honk.sigxcpu.org/projects/git-buildpackage/manual-html/gbp.html"
class="uri">http://honk.sigxcpu.org/projects/git-buildpackage/manual-html/gbp.html</a></li>
<li><a
href="http://www.debian-administration.org/article/Rolling_your_own_Debian_packages_part_1"
class="uri">http://www.debian-administration.org/article/Rolling_your_own_Debian_packages_part_1</a></li>
</ul>
