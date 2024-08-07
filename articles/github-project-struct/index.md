---
title: github project structure best practices
subtitle: An opinionated guide to structuring repositories
date: 2021-07-11
...

# Introduction

These are my notes on the structural best practices I've been
following in my github repositories. We first go through the global
practices that apply to all kinds of projects, and then we cover the
language-dependent ones. This is definitely not supposed to be a
complete guide - rather, I'm documenting what I do once I have at
least 2 repositories of a specific kind.


# Global practices

These are the practices that apply to almost all kinds of
repositories.


## Workflow

Leave *main* as the default HEAD of the repository - that's the one
that third parties get when they clone/check-out and the base of pull
requests.

Use a *devel* branch as a staging ground for commits. We can then
freely amend commits and push to this branch to make the complete CI
suite analyse it. Once a commit is good to go, just move the *main*
branch over to the latest commit with
`git push origin origin/devel:main`.

We can use other upstream branches to keep drafts of future work, just
be aware that they might start looking like
[feature branches](https://martinfowler.com/bliki/FeatureBranch.html),
which are not entirely ideal.


## Files

A quick rundown of files that are expected in all repositories:

+ `README.md`: markdown file that's rendered at the project's landing
  page.
+ `LICENSE`: the software's license. Yes, we need one in every
  repository, if only to say that we are not liable for anything and
  that what we've written is not fit for any specific purpose. I
  mostly use MIT, which achieves both and doesn't require a bit header
  in every other file.
+ `AUTHORS`: list of authors, required by some packaging tools.
+ `.gitignore`: files that should be ignored by git, usually the files
  generated by the build system.
+ `.gitattributes`: can be used to configure some files to be ignored by
  [git archive](https://git-scm.com/docs/git-archive), which is a
  command that generates a .tar.gz with the contents of the
  repository. We should usually add the files that set up CI, and any
  other files that don't make sense outside of the repository.
+ *man page* for the command-line utilities. It can be generated by
  the build system, though.


## CI


### Provider: github actions

Use a single [github actions](https://github.com/features/actions)
workflow with as many jobs as necessary. That makes job execution and
dependencies easier to visualize and maintain than using multiple
workflows.

I have used [travis-ci](https://www.travis-ci.com/) as a best practice
for almost 10 years before they
[changed their pricing model](https://blog.travis-ci.com/2020-11-02-travis-ci-new-billing)
and added limits to public repositories. They might have gone back on
this, but they lost my trust.


### Test coverage

Getting a 100% of tests passing is only meaningful if the tests are
good. One easy way to get a sense of how good the tests are is by
measuring test code coverage.

Use [coveralls](https://coveralls.io/) for code coverage reports. It
has its quirks, but it can be made to work fine with just about any
language - and it supports build matrices natively.

I also used *codecov* in some more recent repositories. It didn't work
very well with my [workflow](#workflow) in the past, but it seems to
be working better than coveralls lately.

Add a test result and a coverage shield to the top of `README.md`.
That allows visitors to see that you project cares about test coverage
at a glance.


### Formatter

Use a tool to check that files are well formatted - the stricter the
tool is, the better. This may sound a bit radical, but the truth is
that they end up making the code easier to work with for everybody,
and prevent silly conflicts.

Try to integrate the same tool in the code editor so that the
formatting doesn't have to be fixed after the commit is
pushed. Keeping the format can otherwise become a chore.


### omnilint

[omnilint](https://github.com/lpenz/omnilint)<sup>[disclaimer]</sup>
is a github action that performs static analysis on standalone
files. Add it as a first job in all repositories:

```{yml}
jobs:
  omnilint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: docker://lpenz/omnilint:v0.2
```

This makes sure that a minimum standard is kept even before the build
and tests are developed. It checks, for instance, that python files
can be loaded by the interpreter, without actually executing them
(which would require installing dependencies, etc.)


### Build/test matrices

github actions have build matrices support that we can use to test the
project in different environments - architectures and compilers.

Use matrices to test the environments where the project can be
actually deployed - there's no need to go crazy and add all possible
combinations.

Use them also to test in a *future* or *nightly* environment, so that
we can get a heads up about upcoming language changes.


## CD


### Deploy on `main` or on tagging

With regards to deployment, there are two options:

+ deploy commits when they become the HEAD of the `main` branch
  (branch-based);
+ deploy commits when they get tagged (tag-based).

These are not mutually exclusive - we can actually use both in the
same project if it makes sense, separating them in different artifact
repositories. That allows users to what they want with regards to
stability versus frequency of updates.

Use [semver](https://semver.org/spec/v2.0.0.html), with versions in
the format `MAJOR.MINOR.PATCH`. The git tag is usually the version
prefixed by a `v` (`vMAJOR.MINOR.PATCH`). The meaning of the numbers,
according to semver:

+ *MAJOR*: API-incompatible changes;
+ *MINOR*: features;
+ *PATCH*: bug fixes.

This is the whole versioning scheme in tag-based deployments.

Branch-based deployments also use semver tags as checkpoints, but they
deploy using a version in the format `MAJOR.MINOR.PATCH-DISTANCE`,
where `DISTANCE` is the number of commits since the tag
`MAJOR.MINOR.PATCH`.

To generate the versions, use the
[version-generator](https://github.com/marketplace/actions/version-generator)
<sup>[disclaimer]</sup>
github action. It allows us to simplify the deploy condition with
something similar to:

```{yml}
(...)
      - id: version
        uses: docker://lpenz/ghaction-version-gen:0.3
      - name: deploy
        uses: <deploy action>
        if: steps.version.outputs.version_commit != ''
        with:
          version: #dollar#{{ steps.version.outputs.version_commit }}
```

That deploys on commits. To deploy on tags, use `version_tagged`
instead of `version_commit`. The action is also able to check if the
tag matches the version in the language-specific project file, for
some languages.


### Debian on packagecloud

Package your projects in a format supported by distributions, such as
`.deb` or `.rpm` - that makes cross-language dependencies manageable.
Deploy the packages to a repository and use it as an upstream
source. [packagecloud](https://packagecloud.io/) is a very convenient
option for that, and it takes both formats mentioned.

Use the
[Deploy to packagecloud.io](https://github.com/marketplace/actions/deploy-to-packagecloud-io)
<sup>[disclaimer]</sup>
github action, and add a corresponding shield to `README.md`.


### Nix

TODO


### Project-specific

Also deploy to a specific location for the kind of project you created:

+ [crates](https://crates.io/) for rust
+ [pypi](https://pypi.org/) for python
+ [github marketplace](https://github.com/marketplace?type=actions)
  for github actions

This is specially important if the project is not a standalone
executable.

Most language-specific repositories provide a shield that should be
added to `README.md`.


# Rust

**Example**: <https://github.com/lpenz/ogle/>

The rust ecosystem is at just the right point - it has most of the
problems already solved, and few solutions are already obsolete,
making the state-of-the-art easy to find.

We have proper github actions to run tests with coverage, the
formatter, static analyser (clippy), create .deb and language-specific
packages (crates). Use this github action workflow as a base:
<https://github.com/lpenz/ogle/blob/main/.github/workflows/ci.yml>

Deploy to the language-specific repository at <https://crates.io/>,
and add the corresponding shield to `README.md`.

Write a manpage if the project is a command line utility.

Some issues pending:

+ man page generation: the man page is generated in a directory with a
  randomly generated name, and we have to copy it manually before
  generating the .deb.


# C/C++

**Example**: <https://github.com/lpenz/execpermfix/>

C/C++ is on the other end of the spectrum: it's a very old language,
older than even the concept of ecosystem. Every old problem has been
solved multiple times, and some new problems might not even be solved
yet - package management is the most important one.

That said, all the basics can be covered:

+ Use
  [clang-format-lint](https://github.com/marketplace/actions/clang-format-lint)
  for the format checking.
+ Use [cmake](https://cmake.org/) as the build tool. I honestly thing
  it's a bad tool with way too many quirks and secret incantations,
  but it's the de facto standard, and it can do everything when enough
  pressure is applied.
+ Use the
  [cmake-swiss-army-knive](https://github.com/marketplace/actions/cmake-swiss-army-knife)
  <sup>[disclaimer]</sup>
  github action and get a lot of different test types for free, as long
  as the `CMakeLists.txt` file supports them.
+ Provide a basic man page for executables. It's not hard to write one
  by hand, the format doesn't even allow it to get complex.
+ Provide [pkg-config](https://en.wikipedia.org/wiki/Pkg-config) files
  for libraries. They can be used to pass down required compilation
  flags and library dependencies to the users of the library, so that
  they don't have to figure those out and maintain them.

We can use cmake to generate the install target and the .deb from
that.

Use this workflow as the base:
<https://github.com/lpenz/execpermfix/blob/main/.github/workflows/ci.yml>


# Python

**Example**: <https://github.com/lpenz/ftpsmartsync/>

Python is known for having "batteries included", but as another
language that's been around for a while, those batteries had to be
changed several times. We have all the basics covered, including
packaging, but sometimes it's not easy to find the *current* best
practice - not even on stackoverflow - due to answers that are plain
old.

Use python 3 - python 2's end-of-life was on 2020-04-20.

Use `pyproject.toml` and `setup.cfg` instead of `setup.py`. It's not
hard to make the conversion, as long as there's not a lot of dynamic
generation. This is the current (very superficial) official doc:
<https://packaging.python.org/tutorials/packaging-projects/>

Use `stdeb` to create debian packages. It's not perfect, as there's
some friction with the way `share/doc` files are deployed, but does
the job.

Use the workflow at
<https://github.com/lpenz/ftpsmartsync/blob/main/.github/workflows/ci.yml>
as a base.


# github actions

**Example**: <https://github.com/lpenz/ghaction-cmake/>

We don't have that many resources for github actions, to be honest.

Create *docker* github actions if possible, as they are
self-contained, easier to test and tend to be faster.

Deploy the containers to <https://hub.docker.com/> using the
[docker/build-push-action](https://github.com/marketplace/actions/build-and-push-docker-images).
Docker Hub can automatically build containers, but it has a quota, and
using actions allows tests jobs to run before deployment.

To create a new version of a github action, tag the repository as
usual, let the container be deployed to docker hub and then use
github's `Create a new release` link. This last step updates github's
marketplace information, and can't automated.

Add the following shields to `README.md`:

+ marketplace link
+ github release
+ docker hub release

That simplifies checking if the latest releases are in sync.

Note: don't CD github actions from the `main` branch, as we have the
extra steps mentioned above.


# Final remarks

This is probably the document that will get updated the most as we
figure out better ways to do things.

In short, we should strive to get the following whenever possible:

+ github actions
+ omnilint
+ automatic formatting
+ coverage with README.md shield
+ debian packaging and deployment from tags, with shield
+ language-specific deployment from tags (specially for libraries),
  with shield

The table below summarizes the language-specific tools that support
our scheme:

<table class="table">
<thead>
<tr>
<th>Category</th>
<th>Formatter</th>
<th>Debian</th>
<th>Deploy</th>
<th>Shields</th>
<th>Example</th>
</tr>
</thead>
<tbody>
<tr>
<td>[Rust](#rust)</td>
<td>[rustfmt](https://crates.io/crates/rustfmt-nightly)</td>
<td>[cargo-deb](https://docs.rs/crate/cargo-deb/)</td>
<td>[packagecloud.io](https://packagecloud.io/)</td>
<td>
+ coverage
+ crates.io release
+ packagecloud.io
</td>
<td>[ogle](https://github.com/lpenz/ogle/blob/main/.github/workflows/ci.yml)</td>
</tr>
<tr>
<td>[C/C++](#cc)</td>
<td>[clang-format](https://clang.llvm.org/docs/ClangFormat.html)</td>
<td>[cpack](https://cmake.org/cmake/help/latest/manual/cpack.1.html)</td>
<td>[packagecloud.io](https://packagecloud.io/)</td>
<td>
+ coverage
+ packagecloud.io
</td>
<td>[execpermfix](https://github.com/lpenz/execpermfix/blob/main/.github/workflows/ci.yml)</td>
</tr>
<tr>
<td>[Python](#python)</td>
<td>[black](https://pypi.org/project/black/)</td>
<td>[stdeb](https://pypi.org/project/stdeb/)</td>
<td>[packagecloud.io](https://packagecloud.io/)</td>
<td>
+ coverage
+ packagecloud.io
</td>
<td>[ftpsmartsync](https://github.com/lpenz/ftpsmartsync/blob/main/.github/workflows/ci.yml)</td>
</tr>
<tr>
<td>[ghaction](#github-actions)</td>
<td>No</td>
<td>N/A</td>
<td>[Docker Hub](https://hub.docker.com/)</td>
<td>
+ marketplace
+ github release
+ dockerhub release
</td>
<td>[ghaction-cmake](https://github.com/lpenz/ghaction-cmake/blob/main/.github/workflows/ci.yml)</td>
</tr>
</tbody>
</table>


## Disclaimer

Be aware that I've tagged github actions that I created with a
<sup>[disclaimer]</sup> tag that links to this section.

I had created some github actions before starting this document, and I
ended up creating a few more while write writing it. I avoided doing
that, though, but sometimes the sortcomings were too severe and/or
very easy to overcome.


[disclaimer]: #disclaimer
