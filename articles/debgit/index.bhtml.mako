<div class="body" id="body">
<p>
<strong>Updated 2018-04-03</strong>: <em>git-buildpackage</em>'s commands have changed, so this
article had to be fixed; I took the opportunity to improve a few things as
well.
</p>
<p>
After building some useful piece of software, one has to decide how to best
deploy it. In UNIX, the standard way to do that is by publishing the source
code in .tar.gz format and requiring users to compile it.
</p>
<p>
In Debian there is an alternative: using a .deb package. With a .deb package, a
single <code>dpkg -i <%text>${PACKAGE}</%text>.deb</code> installs the software.
</p>
<p>
This article explains how to create and support a .deb package for a simple
software maintained in git, by tracking the packaging scheme in a specific
branch on the same repository.
</p>

<section>
<h1>Prerequisites</h1>

<p>
In order to ease the packaging and keep our package warning-free, it should
have in its main repository:
</p>

<ul>
<li>An <code>AUTHORS</code> file with copyright information.
</li>
<li>A manual: <code><%text>${PACKAGE}</%text>.1</code> or similar.
</li>
<li>A <code>COPYING</code> file with GPL information or some other license.
</li>
<li>An appropriate build file for the package. For C/C++ programs, I recommend
  using <code>cmake</code>; for python, <code>setup.py</code>, etc.
</li>
</ul>

<p>
These items are not debian-specific and are useful for everyone.
</p>

</section>
<section>
<h1>Initial packaging setup</h1>

<p>
The first step is creating the <code><%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>.orig.tar.gz</code> file. You
can use git itself for that, by running the following commands in the
repository:
</p>

<pre>
PREFIX=<%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>
git archive --format=tar --prefix=$PREFIX/ $VERSION | gzip -c &gt; ../$PREFIX.orig.tar.gz
</pre>

<p>
You can check the contents of the archive with <em>tar</em>. If there are extraneous
files in the archive, you can configure git-archove to exclude them by creating
a <code>.gitattributes</code> file; for example:
</p>

<pre>
.gitignore      export-ignore
.gitattributes  export-ignore
.travis.yml     export-ignore
</pre>

<p>
The next step is to create the debian branches in the git repository: on the
debian-upstream branch, we store the upstream source, while the debian-debian
branch holds the debian package data.
This separation provides a cleaner revision history by separating the changes
that affect the software from the changes in the packaging.
</p>
<p>
In order to create these branches, we issue the following commands in the git
repository:
</p>

<pre>
git checkout --orphan debian-upstream
git rm --cached -r .
git clean -xfd
git commit --allow-empty -m 'Start of debian branches.'
git checkout -b debian-debian
</pre>

<p>
That creates both branches as orphans, pointing to an empty root commit.
</p>
<p>
We now use the <code>../<%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>.orig.tar.gz</code> file to create the
initial <code>debian</code> directory in the debian-debian branch:
</p>

<pre>
dh_make -s -p <%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>
</pre>

<p>
We can now customize the standard <code>debian</code> directory created. You must edit
the following files: <code>changelog</code>, <code>control</code>, <code>copyright</code> and <code>rules</code>.
Besides those, the <code>compat</code> file must be present; the other files can be
safely removed.
</p>
<p>
After changing the files that <code>dh_make</code> created, you should create a
<code>debian/gbp.conf</code> with the following contents:
</p>

<pre>
[DEFAULT]
upstream-branch=debian-upstream
debian-branch=debian-debian
</pre>

<p>
We can now commit the debian directory in the debian-debian branch.
</p>

</section>
<section>
<h1>Importing the sources</h1>

<p>
In the debian-debian branch:
</p>

<pre>
gbp import-orig --no-interactive ../<%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>.orig.tar.gz
</pre>

<p>
That imports the original sources to the debian-upstream branch, and merge
it into the debian-debian branch.
</p>

</section>
<section>
<h1>Creating the package</h1>

<p>
To create the debian package:
</p>

<pre>
gbp buildpackage -us -uc --git-tag
</pre>

</section>
<section>
<h1>Importing further versions</h1>

<p>
Create the new <code>../<%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>/.orig.tar.gz</code> and then:
</p>

<pre>
gbp import-orig --no-interactive ../<%text>${PACKAGE}</%text>_<%text>${VERSION}</%text>.orig.tar.gz
</pre>

<p>
Edit the <code>debian/changelog</code> file (we can use <code>dch -i -v $VERSION</code> for
that), and create a new package:
</p>

<pre>
gbp buildpackage -us -uc --git-tag
</pre>

<p>
Yes, it's that easy.
</p>

</section>
<section>
<h1>Final remarks</h1>

<p>
After an initial expensive setup, package creation of further versions is
mostly painless, which is the whole point of git-buildpackage and friends.
</p>
<p>
Besides this article, we should check the <code>debian</code> dir of some already packaged
software for reference. We can look at the
<a href="https://github.com/lpenz/execpermfix">execpermfix</a> repository at
<a href="https://github.com">github</a> when first trying to package something.
</p>
<p>
Further information:
</p>

<ul>
<li><a href="https://www.eyrie.org/~eagle/notes/debian/git.html">https://www.eyrie.org/~eagle/notes/debian/git.html</a>
</li>
<li><a href="http://honk.sigxcpu.org/projects/git-buildpackage/manual-html/gbp.html">http://honk.sigxcpu.org/projects/git-buildpackage/manual-html/gbp.html</a>
</li>
<li><a href="http://www.debian-administration.org/article/Rolling_your_own_Debian_packages_part_1">http://www.debian-administration.org/article/Rolling_your_own_Debian_packages_part_1</a>
</li>
</ul>

</section>
</div>
