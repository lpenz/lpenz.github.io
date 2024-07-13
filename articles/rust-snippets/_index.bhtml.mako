<div id="toc">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#introduction"><span class="toc-section-number">1</span>
Introduction</a></li>
<li><a href="#top-level"><span class="toc-section-number">2</span> Top
level</a>
<ul>
<li><a href="#tool-specific-main-function"><span
class="toc-section-number">2.1</span> Tool-specific <code>main</code>
function</a></li>
<li><a href="#top-level-main-function"><span
class="toc-section-number">2.2</span> Top-level <code>main</code>
function</a></li>
<li><a href="#crates"><span class="toc-section-number">2.3</span>
Crates</a></li>
<li><a href="#async-tool-with-tokio"><span
class="toc-section-number">2.4</span> Async tool with tokio</a></li>
</ul></li>
<li><a href="#tests"><span class="toc-section-number">3</span>
Tests</a></li>
<li><a href="#errors"><span class="toc-section-number">4</span>
Errors</a>
<ul>
<li><a href="#error-type-creation-crateless"><span
class="toc-section-number">4.1</span> Error type creation,
crateless</a></li>
<li><a href="#error-type-creation-with-thiserror"><span
class="toc-section-number">4.2</span> Error type creation with
<code>thiserror</code></a></li>
<li><a href="#error-handling-with-eyre"><span
class="toc-section-number">4.3</span> Error handling with
<code>eyre</code></a></li>
</ul></li>
<li><a href="#traits-for-instantiation"><span
class="toc-section-number">5</span> Traits for instantiation</a>
<ul>
<li><a href="#default"><span class="toc-section-number">5.1</span>
Default</a></li>
<li><a href="#from"><span class="toc-section-number">5.2</span>
From</a></li>
<li><a href="#tryfrom"><span class="toc-section-number">5.3</span>
TryFrom</a></li>
</ul></li>
<li><a href="#traits-for-string-conversion"><span
class="toc-section-number">6</span> Traits for string conversion</a>
<ul>
<li><a href="#debug"><span class="toc-section-number">6.1</span>
Debug</a></li>
<li><a href="#display"><span class="toc-section-number">6.2</span>
Display</a></li>
<li><a href="#fromstr"><span class="toc-section-number">6.3</span>
FromStr</a></li>
</ul></li>
<li><a href="#sync-io"><span class="toc-section-number">7</span> Sync
I/O</a>
<ul>
<li><a href="#running-external-commands"><span
class="toc-section-number">7.1</span> Running external commands</a></li>
</ul></li>
<li><a href="#async-io-with-tokio"><span
class="toc-section-number">8</span> Async I/O with
<code>tokio</code></a>
<ul>
<li><a href="#running-external-commands-1"><span
class="toc-section-number">8.1</span> Running external commands</a></li>
</ul></li>
<li><a href="#misc-snippets-i.e.-unclassified"><span
class="toc-section-number">9</span> Misc snippets
(i.e. unclassified)</a>
<ul>
<li><a href="#entry"><span class="toc-section-number">9.1</span>
Entry</a></li>
</ul></li>
</ul>
</div>
<h1 data-number="1" id="introduction"><span
class="header-section-number">1</span> Introduction</h1>
<p>This article is essentially a collection of code snippets written in
rust with no clear theme. They include “raw” rust and state-of-the-art
crates.</p>
<p>I’ll probably change this article as things evolve. I plan on
removing snippets about crates that are no longer a consensus, for
intance.</p>
<p>For the examples below, we are using an example crate called
<code>snippets</code>, with a binary called <code>tool</code>.</p>
<h1 data-number="2" id="top-level"><span
class="header-section-number">2</span> Top level</h1>
<h2 data-number="2.1" id="tool-specific-main-function"><span
class="header-section-number">2.1</span> Tool-specific <code>main</code>
function</h2>
<p>When creating a tool, we can write a placeholder
<code>src/bin/tool.rs</code> file that calls a few default
initialization functions that should only be called once, and then calls
a tool-specific <code>main</code> function coming from somewhere
referenced by <code>lib.rs</code>.</p>
<p>This tool-specific <code>main</code> function does all initialization
and looks like the following:</p>
<div class="sourceCode" id="cb1"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb1-1"><a href="#cb1-1" aria-hidden="true" tabindex="-1"></a><span class="co">// Copyright information ...</span></span>
<span id="cb1-2"><a href="#cb1-2" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">clap::</span>Parser<span class="op">;</span></span>
<span id="cb1-3"><a href="#cb1-3" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">color_eyre::</span><span class="dt">Result</span><span class="op">;</span></span>
<span id="cb1-4"><a href="#cb1-4" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-5"><a href="#cb1-5" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>derive<span class="at">(</span>Parser<span class="op">,</span> <span class="bu">Debug</span><span class="at">)]</span></span>
<span id="cb1-6"><a href="#cb1-6" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>command<span class="at">(</span>author<span class="op">,</span> version<span class="op">,</span> about<span class="op">,</span> long_about <span class="op">=</span> <span class="cn">None</span><span class="at">)]</span></span>
<span id="cb1-7"><a href="#cb1-7" aria-hidden="true" tabindex="-1"></a><span class="kw">struct</span> Cli <span class="op">{</span></span>
<span id="cb1-8"><a href="#cb1-8" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Add command-line arguments to this struct, with documentation</span></span>
<span id="cb1-9"><a href="#cb1-9" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span>
<span id="cb1-10"><a href="#cb1-10" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb1-11"><a href="#cb1-11" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span><span class="pp">tracing::</span>instrument<span class="at">]</span></span>
<span id="cb1-12"><a href="#cb1-12" aria-hidden="true" tabindex="-1"></a><span class="kw">pub</span> <span class="kw">fn</span> main() <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span>()<span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb1-13"><a href="#cb1-13" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> args <span class="op">=</span> <span class="pp">Cli::</span>parse()<span class="op">;</span></span>
<span id="cb1-14"><a href="#cb1-14" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Program goes here</span></span>
<span id="cb1-15"><a href="#cb1-15" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Example trace:</span></span>
<span id="cb1-16"><a href="#cb1-16" aria-hidden="true" tabindex="-1"></a>    <span class="pp">tracing::info!</span>(<span class="st">&quot;args struct: {:?}&quot;</span><span class="op">,</span> args)<span class="op">;</span></span>
<span id="cb1-17"><a href="#cb1-17" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Return Ok(()) on success</span></span>
<span id="cb1-18"><a href="#cb1-18" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(())</span>
<span id="cb1-19"><a href="#cb1-19" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<p>We can also put this code in the top-level <code>main</code> function
(example below) when it’s more convenient.</p>
<h2 data-number="2.2" id="top-level-main-function"><span
class="header-section-number">2.2</span> Top-level <code>main</code>
function</h2>
<p>The top-level <code>main</code> function that goes in the
<code>src/bin/tool.rs</code> is pretty much always like the
following:</p>
<div class="sourceCode" id="cb2"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb2-1"><a href="#cb2-1" aria-hidden="true" tabindex="-1"></a><span class="co">// Copyright information ...</span></span>
<span id="cb2-2"><a href="#cb2-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb2-3"><a href="#cb2-3" aria-hidden="true" tabindex="-1"></a><span class="kw">fn</span> main() <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span>()<span class="op">,</span> <span class="dt">Box</span><span class="op">&lt;</span><span class="kw">dyn</span> <span class="pp">std::error::</span><span class="bu">Error</span><span class="op">&gt;&gt;</span> <span class="op">{</span></span>
<span id="cb2-4"><a href="#cb2-4" aria-hidden="true" tabindex="-1"></a>    <span class="pp">color_eyre::</span>install()<span class="op">?;</span></span>
<span id="cb2-5"><a href="#cb2-5" aria-hidden="true" tabindex="-1"></a>    <span class="pp">tracing_subscriber::</span>fmt()</span>
<span id="cb2-6"><a href="#cb2-6" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>with_span_events(<span class="pp">tracing_subscriber::fmt::format::FmtSpan::</span>ACTIVE)</span>
<span id="cb2-7"><a href="#cb2-7" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>with_env_filter(<span class="pp">tracing_subscriber::EnvFilter::</span>from_default_env())</span>
<span id="cb2-8"><a href="#cb2-8" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>init()<span class="op">;</span></span>
<span id="cb2-9"><a href="#cb2-9" aria-hidden="true" tabindex="-1"></a>    <span class="pp">snippets::</span>main()<span class="op">?;</span></span>
<span id="cb2-10"><a href="#cb2-10" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(())</span>
<span id="cb2-11"><a href="#cb2-11" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<p>It’s worth making this <code>main</code> function the only
<code>pub</code> in the crate if you want to use a standard CI that
includes <a
href="https://crates.io/crates/cargo-semver-checks">cargo-semver-checks</a>
like <a
href="https://github.com/lpenz/ghworkflow-rust">ghworkflow-rust</a>.</p>
<h2 data-number="2.3" id="crates"><span
class="header-section-number">2.3</span> Crates</h2>
<p>For these the functions above to compile, we need some crates:</p>
<div class="sourceCode" id="cb3"><pre
class="sourceCode bash"><code class="sourceCode bash"><span id="cb3-1"><a href="#cb3-1" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add clap <span class="at">--features</span> derive</span>
<span id="cb3-2"><a href="#cb3-2" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add tracing</span>
<span id="cb3-3"><a href="#cb3-3" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add tracing-subscriber <span class="at">--features</span> env-filter,tracing-log</span>
<span id="cb3-4"><a href="#cb3-4" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add color_eyre</span></code></pre></div>
<p>What we get by using the batteries-included <code>main</code>
above:</p>
<ul>
<li><a href="https://docs.rs/clap/latest/clap/">clap</a> creates a
command-line argument parser from the struct, with type checking and
help derived from the documentation.</li>
<li><a href="https://docs.rs/tracing/latest/tracing/">tracing</a>
provides the <code>#[instrument]</code> attribute, which makes adding
logs very convenient and is also used by <code>color_eyre</code> for
error reporting.</li>
<li><a
href="https://docs.rs/color-eyre/latest/color_eyre/">color_eyre</a> an
error report handler that captures span traces provided by
<code>#[instrument]</code> and has a colorful output. This effectively
prints something similar to a backtrace when errors are not handled and
“bubble up” to <code>main</code>.</li>
<li><a
href="https://docs.rs/tracing-subscriber/latest/tracing_subscriber">tracing-subscriber</a>
sets up the destination of the traces. In the particular case above, we
are configuring the filter using the <a
href="https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html"><code>RUST_LOG</code></a>
environment variable, and printing the messages to <code>stderr</code>
when they are enabled.</li>
</ul>
<p>It’s worth noting that there is a whole lot that can be done with
tracing, including directing it to <a
href="https://docs.rs/tracing-opentelemetry/latest/tracing_opentelemetry/">OpenTelemetry</a>
and/or using it for <a
href="https://docs.rs/tracing-timing/latest/tracing_timing/">profiling</a>,
maybe with <a
href="https://crates.io/crates/tracing-flame">flamegraphs</a>.</p>
<p>Be aware, though, that the setup above directs logs to the tracing
infra and not the other way around - meaning that if we increment the
setup above to direct traces to the logging infra, we then effectively
set up a tracing-logging loop.</p>
<h2 data-number="2.4" id="async-tool-with-tokio"><span
class="header-section-number">2.4</span> Async tool with tokio</h2>
<p>Async versions are not so different. Add the crate:</p>
<div class="sourceCode" id="cb4"><pre
class="sourceCode bash"><code class="sourceCode bash"><span id="cb4-1"><a href="#cb4-1" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add tokio <span class="at">--features</span><span class="op">=</span>macros,rt,rt-multi-thread</span></code></pre></div>
<p>Add <code>async</code> to the tool-specific <code>main</code>:</p>
<div class="sourceCode" id="cb5"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb5-1"><a href="#cb5-1" aria-hidden="true" tabindex="-1"></a><span class="co">// Copyright information ...</span></span>
<span id="cb5-2"><a href="#cb5-2" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">clap::</span>Parser<span class="op">;</span></span>
<span id="cb5-3"><a href="#cb5-3" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">color_eyre::</span><span class="dt">Result</span><span class="op">;</span></span>
<span id="cb5-4"><a href="#cb5-4" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb5-5"><a href="#cb5-5" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>derive<span class="at">(</span>Parser<span class="op">,</span> <span class="bu">Debug</span><span class="at">)]</span></span>
<span id="cb5-6"><a href="#cb5-6" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>command<span class="at">(</span>author<span class="op">,</span> version<span class="op">,</span> about<span class="op">,</span> long_about <span class="op">=</span> <span class="cn">None</span><span class="at">)]</span></span>
<span id="cb5-7"><a href="#cb5-7" aria-hidden="true" tabindex="-1"></a><span class="kw">struct</span> Cli <span class="op">{</span></span>
<span id="cb5-8"><a href="#cb5-8" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Add command-line arguments to this struct, with documentation</span></span>
<span id="cb5-9"><a href="#cb5-9" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span>
<span id="cb5-10"><a href="#cb5-10" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb5-11"><a href="#cb5-11" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span><span class="pp">tracing::</span>instrument<span class="at">]</span></span>
<span id="cb5-12"><a href="#cb5-12" aria-hidden="true" tabindex="-1"></a><span class="kw">pub</span> <span class="kw">async</span> <span class="kw">fn</span> main() <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span>()<span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb5-13"><a href="#cb5-13" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> args <span class="op">=</span> <span class="pp">Cli::</span>parse()<span class="op">;</span></span>
<span id="cb5-14"><a href="#cb5-14" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Program goes here</span></span>
<span id="cb5-15"><a href="#cb5-15" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Example trace:</span></span>
<span id="cb5-16"><a href="#cb5-16" aria-hidden="true" tabindex="-1"></a>    <span class="pp">tracing::info!</span>(<span class="st">&quot;args struct: {:?}&quot;</span><span class="op">,</span> args)<span class="op">;</span></span>
<span id="cb5-17"><a href="#cb5-17" aria-hidden="true" tabindex="-1"></a>    <span class="co">// Return Ok(()) on success</span></span>
<span id="cb5-18"><a href="#cb5-18" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(())</span>
<span id="cb5-19"><a href="#cb5-19" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<p>And add <code>#[tokio::main]</code> and <code>async</code> to the
top-level <code>main</code>:</p>
<div class="sourceCode" id="cb6"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb6-1"><a href="#cb6-1" aria-hidden="true" tabindex="-1"></a><span class="co">// Copyright information ...</span></span>
<span id="cb6-2"><a href="#cb6-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb6-3"><a href="#cb6-3" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span><span class="pp">tokio::</span>main<span class="at">]</span></span>
<span id="cb6-4"><a href="#cb6-4" aria-hidden="true" tabindex="-1"></a><span class="kw">async</span> <span class="kw">fn</span> main() <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span>()<span class="op">,</span> <span class="dt">Box</span><span class="op">&lt;</span><span class="kw">dyn</span> <span class="pp">std::error::</span><span class="bu">Error</span><span class="op">&gt;&gt;</span> <span class="op">{</span></span>
<span id="cb6-5"><a href="#cb6-5" aria-hidden="true" tabindex="-1"></a>    <span class="pp">color_eyre::</span>install()<span class="op">?;</span></span>
<span id="cb6-6"><a href="#cb6-6" aria-hidden="true" tabindex="-1"></a>    <span class="pp">tracing_subscriber::</span>fmt()</span>
<span id="cb6-7"><a href="#cb6-7" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>with_span_events(<span class="pp">tracing_subscriber::fmt::format::FmtSpan::</span>ACTIVE)</span>
<span id="cb6-8"><a href="#cb6-8" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>with_env_filter(<span class="pp">tracing_subscriber::EnvFilter::</span>from_default_env())</span>
<span id="cb6-9"><a href="#cb6-9" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>init()<span class="op">;</span></span>
<span id="cb6-10"><a href="#cb6-10" aria-hidden="true" tabindex="-1"></a>    <span class="pp">snippets::</span>main()<span class="op">.</span><span class="kw">await</span><span class="op">?;</span></span>
<span id="cb6-11"><a href="#cb6-11" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(())</span>
<span id="cb6-12"><a href="#cb6-12" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<p>Again, it’s worth making this <code>main</code> function the only
<code>pub</code> in the crate if you want to use a standard CI that
includes <a
href="https://crates.io/crates/cargo-semver-checks">cargo-semver-checks</a>
like <a
href="https://github.com/lpenz/ghworkflow-rust">ghworkflow-rust</a>.</p>
<h1 data-number="3" id="tests"><span
class="header-section-number">3</span> Tests</h1>
<p>To be able to see the traces in tests, the easiest way is to use:</p>
<div class="sourceCode" id="cb7"><pre
class="sourceCode bash"><code class="sourceCode bash"><span id="cb7-1"><a href="#cb7-1" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add <span class="at">--dev</span> env_logger</span>
<span id="cb7-2"><a href="#cb7-2" aria-hidden="true" tabindex="-1"></a><span class="ex">cargo</span> add <span class="at">--dev</span> test-log</span></code></pre></div>
<p><code>test-log</code> takes care of initializing the tracing infra in
accordance with the <a
href="https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html"><code>RUST_LOG</code></a>
environment variable when we use its <code>test</code> macro.</p>
<h1 data-number="4" id="errors"><span
class="header-section-number">4</span> Errors</h1>
<h2 data-number="4.1" id="error-type-creation-crateless"><span
class="header-section-number">4.1</span> Error type creation,
crateless</h2>
<ul>
<li>Built-in error trait: <a
href="https://doc.rust-lang.org/std/error/trait.Error.html"
class="uri">https://doc.rust-lang.org/std/error/trait.Error.html</a></li>
<li>Example adapted from: <a
href="https://web.mit.edu/rust-lang_v1.25/arch/amd64_ubuntu1404/share/doc/rust/html/book/first-edition/error-handling.html"
class="uri">https://web.mit.edu/rust-lang_v1.25/arch/amd64_ubuntu1404/share/doc/rust/html/book/first-edition/error-handling.html</a></li>
</ul>
<div class="sourceCode" id="cb8"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb8-1"><a href="#cb8-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span>error<span class="op">;</span></span>
<span id="cb8-2"><a href="#cb8-2" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span>fmt<span class="op">;</span></span>
<span id="cb8-3"><a href="#cb8-3" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span>io<span class="op">;</span></span>
<span id="cb8-4"><a href="#cb8-4" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span>num<span class="op">;</span></span>
<span id="cb8-5"><a href="#cb8-5" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb8-6"><a href="#cb8-6" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>derive<span class="at">(</span><span class="bu">Debug</span><span class="at">)]</span></span>
<span id="cb8-7"><a href="#cb8-7" aria-hidden="true" tabindex="-1"></a><span class="kw">enum</span> MyError <span class="op">{</span></span>
<span id="cb8-8"><a href="#cb8-8" aria-hidden="true" tabindex="-1"></a>    Io(<span class="pp">io::</span><span class="bu">Error</span>)<span class="op">,</span></span>
<span id="cb8-9"><a href="#cb8-9" aria-hidden="true" tabindex="-1"></a>    Parse(<span class="pp">num::</span>ParseIntError)<span class="op">,</span></span>
<span id="cb8-10"><a href="#cb8-10" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span>
<span id="cb8-11"><a href="#cb8-11" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb8-12"><a href="#cb8-12" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="pp">fmt::</span><span class="bu">Display</span> <span class="cf">for</span> MyError <span class="op">{</span></span>
<span id="cb8-13"><a href="#cb8-13" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> fmt(<span class="op">&amp;</span><span class="kw">self</span><span class="op">,</span> f<span class="op">:</span> <span class="op">&amp;</span><span class="kw">mut</span> <span class="pp">fmt::</span>Formatter) <span class="op">-&gt;</span> <span class="pp">fmt::</span><span class="dt">Result</span> <span class="op">{</span></span>
<span id="cb8-14"><a href="#cb8-14" aria-hidden="true" tabindex="-1"></a>        <span class="cf">match</span> <span class="op">*</span><span class="kw">self</span> <span class="op">{</span></span>
<span id="cb8-15"><a href="#cb8-15" aria-hidden="true" tabindex="-1"></a>            <span class="pp">MyError::</span>Io(<span class="kw">ref</span> err) <span class="op">=&gt;</span> <span class="pp">write!</span>(f<span class="op">,</span> <span class="st">&quot;IO error: {}&quot;</span><span class="op">,</span> err)<span class="op">,</span></span>
<span id="cb8-16"><a href="#cb8-16" aria-hidden="true" tabindex="-1"></a>            <span class="pp">MyError::</span>Parse(<span class="kw">ref</span> err) <span class="op">=&gt;</span> <span class="pp">write!</span>(f<span class="op">,</span> <span class="st">&quot;Parse error: {}&quot;</span><span class="op">,</span> err)<span class="op">,</span></span>
<span id="cb8-17"><a href="#cb8-17" aria-hidden="true" tabindex="-1"></a>        <span class="op">}</span></span>
<span id="cb8-18"><a href="#cb8-18" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb8-19"><a href="#cb8-19" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span>
<span id="cb8-20"><a href="#cb8-20" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb8-21"><a href="#cb8-21" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="pp">error::</span><span class="bu">Error</span> <span class="cf">for</span> MyError <span class="op">{</span></span>
<span id="cb8-22"><a href="#cb8-22" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> source(<span class="op">&amp;</span><span class="kw">self</span>) <span class="op">-&gt;</span> <span class="dt">Option</span><span class="op">&lt;&amp;</span>(<span class="kw">dyn</span> <span class="pp">error::</span><span class="bu">Error</span> <span class="op">+</span> <span class="ot">&#39;static</span>)<span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb8-23"><a href="#cb8-23" aria-hidden="true" tabindex="-1"></a>        <span class="cf">match</span> <span class="op">*</span><span class="kw">self</span> <span class="op">{</span></span>
<span id="cb8-24"><a href="#cb8-24" aria-hidden="true" tabindex="-1"></a>            <span class="pp">MyError::</span>Io(<span class="kw">ref</span> err) <span class="op">=&gt;</span> <span class="cn">Some</span>(err)<span class="op">,</span></span>
<span id="cb8-25"><a href="#cb8-25" aria-hidden="true" tabindex="-1"></a>            <span class="pp">MyError::</span>Parse(<span class="kw">ref</span> err) <span class="op">=&gt;</span> <span class="cn">Some</span>(err)<span class="op">,</span></span>
<span id="cb8-26"><a href="#cb8-26" aria-hidden="true" tabindex="-1"></a>        <span class="op">}</span></span>
<span id="cb8-27"><a href="#cb8-27" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb8-28"><a href="#cb8-28" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span>
<span id="cb8-29"><a href="#cb8-29" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb8-30"><a href="#cb8-30" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="bu">From</span><span class="op">&lt;</span><span class="pp">io::</span><span class="bu">Error</span><span class="op">&gt;</span> <span class="cf">for</span> MyError <span class="op">{</span></span>
<span id="cb8-31"><a href="#cb8-31" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> from(err<span class="op">:</span> <span class="pp">io::</span><span class="bu">Error</span>) <span class="op">-&gt;</span> MyError <span class="op">{</span></span>
<span id="cb8-32"><a href="#cb8-32" aria-hidden="true" tabindex="-1"></a>        <span class="pp">MyError::</span>Io(err)</span>
<span id="cb8-33"><a href="#cb8-33" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb8-34"><a href="#cb8-34" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span>
<span id="cb8-35"><a href="#cb8-35" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb8-36"><a href="#cb8-36" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="bu">From</span><span class="op">&lt;</span><span class="pp">num::</span>ParseIntError<span class="op">&gt;</span> <span class="cf">for</span> MyError <span class="op">{</span></span>
<span id="cb8-37"><a href="#cb8-37" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> from(err<span class="op">:</span> <span class="pp">num::</span>ParseIntError) <span class="op">-&gt;</span> MyError <span class="op">{</span></span>
<span id="cb8-38"><a href="#cb8-38" aria-hidden="true" tabindex="-1"></a>        <span class="pp">MyError::</span>Parse(err)</span>
<span id="cb8-39"><a href="#cb8-39" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb8-40"><a href="#cb8-40" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="4.2" id="error-type-creation-with-thiserror"><span
class="header-section-number">4.2</span> Error type creation with
<code>thiserror</code></h2>
<p><a href="https://docs.rs/thiserror/latest/thiserror/"
class="uri">https://docs.rs/thiserror/latest/thiserror/</a></p>
<div class="sourceCode" id="cb9"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb9-1"><a href="#cb9-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">thiserror::</span><span class="bu">Error</span><span class="op">;</span></span>
<span id="cb9-2"><a href="#cb9-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb9-3"><a href="#cb9-3" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>derive<span class="at">(</span><span class="bu">Error</span><span class="op">,</span> <span class="bu">Debug</span><span class="at">)]</span></span>
<span id="cb9-4"><a href="#cb9-4" aria-hidden="true" tabindex="-1"></a><span class="kw">pub</span> <span class="kw">enum</span> DataStoreError <span class="op">{</span></span>
<span id="cb9-5"><a href="#cb9-5" aria-hidden="true" tabindex="-1"></a>    <span class="at">#[</span>error<span class="at">(</span><span class="st">&quot;data store disconnected&quot;</span><span class="at">)]</span></span>
<span id="cb9-6"><a href="#cb9-6" aria-hidden="true" tabindex="-1"></a>    Disconnect(<span class="at">#[</span>from<span class="at">]</span> <span class="pp">io::</span><span class="bu">Error</span>)<span class="op">,</span></span>
<span id="cb9-7"><a href="#cb9-7" aria-hidden="true" tabindex="-1"></a>    <span class="at">#[</span>error<span class="at">(</span><span class="st">&quot;the data for key `{0}` is not available&quot;</span><span class="at">)]</span></span>
<span id="cb9-8"><a href="#cb9-8" aria-hidden="true" tabindex="-1"></a>    Redaction(<span class="dt">String</span>)<span class="op">,</span></span>
<span id="cb9-9"><a href="#cb9-9" aria-hidden="true" tabindex="-1"></a>    <span class="at">#[</span>error<span class="at">(</span><span class="st">&quot;invalid header (expected {expected:?}, found {found:?})&quot;</span><span class="at">)]</span></span>
<span id="cb9-10"><a href="#cb9-10" aria-hidden="true" tabindex="-1"></a>    InvalidHeader <span class="op">{</span></span>
<span id="cb9-11"><a href="#cb9-11" aria-hidden="true" tabindex="-1"></a>        expected<span class="op">:</span> <span class="dt">String</span><span class="op">,</span></span>
<span id="cb9-12"><a href="#cb9-12" aria-hidden="true" tabindex="-1"></a>        found<span class="op">:</span> <span class="dt">String</span><span class="op">,</span></span>
<span id="cb9-13"><a href="#cb9-13" aria-hidden="true" tabindex="-1"></a>    <span class="op">},</span></span>
<span id="cb9-14"><a href="#cb9-14" aria-hidden="true" tabindex="-1"></a>    <span class="at">#[</span>error<span class="at">(</span><span class="st">&quot;unknown data store error&quot;</span><span class="at">)]</span></span>
<span id="cb9-15"><a href="#cb9-15" aria-hidden="true" tabindex="-1"></a>    Unknown<span class="op">,</span></span>
<span id="cb9-16"><a href="#cb9-16" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="4.3" id="error-handling-with-eyre"><span
class="header-section-number">4.3</span> Error handling with
<code>eyre</code></h2>
<ul>
<li><code>eyre</code> trait: <a href="https://docs.rs/eyre/latest/eyre/"
class="uri">https://docs.rs/eyre/latest/eyre/</a></li>
<li><code>color_eyre</code> handler: <a
href="https://docs.rs/color-eyre/latest/color_eyre/"
class="uri">https://docs.rs/color-eyre/latest/color_eyre/</a></li>
</ul>
<div class="sourceCode" id="cb10"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb10-1"><a href="#cb10-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">color_eyre::</span><span class="op">{</span><span class="pp">eyre::</span>eyre<span class="op">,</span> <span class="dt">Result</span><span class="op">};</span></span>
<span id="cb10-2"><a href="#cb10-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb10-3"><a href="#cb10-3" aria-hidden="true" tabindex="-1"></a><span class="kw">fn</span> get_cluster_info() <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span>ClusterMap<span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb10-4"><a href="#cb10-4" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> config <span class="op">=</span> <span class="pp">std::fs::</span>read_to_string(<span class="st">&quot;cluster.json&quot;</span>)<span class="op">?;</span></span>
<span id="cb10-5"><a href="#cb10-5" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> map<span class="op">:</span> ClusterMap <span class="op">=</span> <span class="pp">serde_json::</span>from_str(<span class="op">&amp;</span>config)<span class="op">?;</span></span>
<span id="cb10-6"><a href="#cb10-6" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> opt <span class="op">=</span> <span class="cn">None</span><span class="op">;</span></span>
<span id="cb10-7"><a href="#cb10-7" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> fromopt <span class="op">=</span> opt<span class="op">.</span>ok_or_else(<span class="op">||</span> <span class="pp">eyre!</span>(<span class="st">&quot;error message&quot;</span>))<span class="op">;</span></span>
<span id="cb10-8"><a href="#cb10-8" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(map)</span>
<span id="cb10-9"><a href="#cb10-9" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h1 data-number="5" id="traits-for-instantiation"><span
class="header-section-number">5</span> Traits for instantiation</h1>
<p>These traits create instances of the type they implement. Consider
the following example type for the sections below:</p>
<div class="sourceCode" id="cb11"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb11-1"><a href="#cb11-1" aria-hidden="true" tabindex="-1"></a><span class="at">#[</span>derive<span class="at">(</span><span class="bu">Debug</span><span class="at">)]</span></span>
<span id="cb11-2"><a href="#cb11-2" aria-hidden="true" tabindex="-1"></a><span class="kw">struct</span> MyType <span class="op">{</span></span>
<span id="cb11-3"><a href="#cb11-3" aria-hidden="true" tabindex="-1"></a>    value<span class="op">:</span> <span class="dt">usize</span><span class="op">,</span></span>
<span id="cb11-4"><a href="#cb11-4" aria-hidden="true" tabindex="-1"></a>    <span class="co">// ...</span></span>
<span id="cb11-5"><a href="#cb11-5" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="5.1" id="default"><span
class="header-section-number">5.1</span> Default</h2>
<p>This can be auto-derived if we want to use the default for all the
members. Otherwise, we have to define it manually.</p>
<p><a href="https://doc.rust-lang.org/std/default/trait.Default.html"
class="uri">https://doc.rust-lang.org/std/default/trait.Default.html</a></p>
<div class="sourceCode" id="cb12"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb12-1"><a href="#cb12-1" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="bu">Default</span> <span class="cf">for</span> MyType <span class="op">{</span></span>
<span id="cb12-2"><a href="#cb12-2" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> default() <span class="op">-&gt;</span> <span class="dt">Self</span> <span class="op">{</span></span>
<span id="cb12-3"><a href="#cb12-3" aria-hidden="true" tabindex="-1"></a>        <span class="dt">Self</span> <span class="op">{</span> value<span class="op">:</span> <span class="dv">10</span> <span class="op">}</span></span>
<span id="cb12-4"><a href="#cb12-4" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb12-5"><a href="#cb12-5" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="5.2" id="from"><span
class="header-section-number">5.2</span> From</h2>
<p><a href="https://doc.rust-lang.org/std/convert/trait.From.html"
class="uri">https://doc.rust-lang.org/std/convert/trait.From.html</a></p>
<div class="sourceCode" id="cb13"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb13-1"><a href="#cb13-1" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="bu">From</span><span class="op">&lt;</span><span class="dt">usize</span><span class="op">&gt;</span> <span class="cf">for</span> MyType <span class="op">{</span></span>
<span id="cb13-2"><a href="#cb13-2" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> from(value<span class="op">:</span> <span class="dt">usize</span>) <span class="op">-&gt;</span> <span class="dt">Self</span> <span class="op">{</span></span>
<span id="cb13-3"><a href="#cb13-3" aria-hidden="true" tabindex="-1"></a>        MyType <span class="op">{</span> value <span class="op">}</span></span>
<span id="cb13-4"><a href="#cb13-4" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb13-5"><a href="#cb13-5" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="5.3" id="tryfrom"><span
class="header-section-number">5.3</span> TryFrom</h2>
<p><a
href="https://doc.rust-lang.org/stable/std/convert/trait.TryFrom.html"
class="uri">https://doc.rust-lang.org/stable/std/convert/trait.TryFrom.html</a></p>
<div class="sourceCode" id="cb14"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb14-1"><a href="#cb14-1" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="bu">TryFrom</span><span class="op">&lt;</span><span class="dt">u32</span><span class="op">&gt;</span> <span class="cf">for</span> MyType <span class="op">{</span></span>
<span id="cb14-2"><a href="#cb14-2" aria-hidden="true" tabindex="-1"></a>    <span class="kw">type</span> Error <span class="op">=</span> <span class="op">&amp;</span><span class="ot">&#39;static</span> <span class="dt">str</span><span class="op">;</span></span>
<span id="cb14-3"><a href="#cb14-3" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> try_from(value<span class="op">:</span> <span class="dt">u32</span>) <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span><span class="dt">Self</span><span class="op">,</span> <span class="dt">Self</span><span class="pp">::</span><span class="bu">Error</span><span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb14-4"><a href="#cb14-4" aria-hidden="true" tabindex="-1"></a>        <span class="cn">Ok</span>(MyType <span class="op">{</span></span>
<span id="cb14-5"><a href="#cb14-5" aria-hidden="true" tabindex="-1"></a>            value<span class="op">:</span> value <span class="kw">as</span> <span class="dt">usize</span><span class="op">,</span></span>
<span id="cb14-6"><a href="#cb14-6" aria-hidden="true" tabindex="-1"></a>        <span class="op">}</span>)</span>
<span id="cb14-7"><a href="#cb14-7" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb14-8"><a href="#cb14-8" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h1 data-number="6" id="traits-for-string-conversion"><span
class="header-section-number">6</span> Traits for string conversion</h1>
<h2 data-number="6.1" id="debug"><span
class="header-section-number">6.1</span> Debug</h2>
<p><a href="https://doc.rust-lang.org/std/fmt/trait.Debug.html"
class="uri">https://doc.rust-lang.org/std/fmt/trait.Debug.html</a></p>
<div class="sourceCode" id="cb15"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb15-1"><a href="#cb15-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span>fmt<span class="op">;</span></span>
<span id="cb15-2"><a href="#cb15-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb15-3"><a href="#cb15-3" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="pp">fmt::</span><span class="bu">Debug</span> <span class="cf">for</span> MyType <span class="op">{</span></span>
<span id="cb15-4"><a href="#cb15-4" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> fmt(<span class="op">&amp;</span><span class="kw">self</span><span class="op">,</span> f<span class="op">:</span> <span class="op">&amp;</span><span class="kw">mut</span> <span class="pp">fmt::</span>Formatter<span class="op">&lt;</span><span class="ot">&#39;_</span><span class="op">&gt;</span>) <span class="op">-&gt;</span> <span class="pp">fmt::</span><span class="dt">Result</span> <span class="op">{</span></span>
<span id="cb15-5"><a href="#cb15-5" aria-hidden="true" tabindex="-1"></a>        f<span class="op">.</span>debug_struct(<span class="st">&quot;MyType&quot;</span>)</span>
<span id="cb15-6"><a href="#cb15-6" aria-hidden="true" tabindex="-1"></a>            <span class="op">.</span>field(<span class="st">&quot;value&quot;</span><span class="op">,</span> <span class="op">&amp;</span><span class="kw">self</span><span class="op">.</span>value)</span>
<span id="cb15-7"><a href="#cb15-7" aria-hidden="true" tabindex="-1"></a>            <span class="op">.</span>finish()</span>
<span id="cb15-8"><a href="#cb15-8" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb15-9"><a href="#cb15-9" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="6.2" id="display"><span
class="header-section-number">6.2</span> Display</h2>
<p><a href="https://doc.rust-lang.org/std/fmt/trait.Display.html"
class="uri">https://doc.rust-lang.org/std/fmt/trait.Display.html</a></p>
<div class="sourceCode" id="cb16"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb16-1"><a href="#cb16-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span>fmt<span class="op">;</span></span>
<span id="cb16-2"><a href="#cb16-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb16-3"><a href="#cb16-3" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="pp">fmt::</span><span class="bu">Display</span> <span class="cf">for</span> MyType <span class="op">{</span></span>
<span id="cb16-4"><a href="#cb16-4" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> fmt(<span class="op">&amp;</span><span class="kw">self</span><span class="op">,</span> f<span class="op">:</span> <span class="op">&amp;</span><span class="kw">mut</span> <span class="pp">fmt::</span>Formatter<span class="op">&lt;</span><span class="ot">&#39;_</span><span class="op">&gt;</span>) <span class="op">-&gt;</span> <span class="pp">fmt::</span><span class="dt">Result</span> <span class="op">{</span></span>
<span id="cb16-5"><a href="#cb16-5" aria-hidden="true" tabindex="-1"></a>        <span class="pp">write!</span>(f<span class="op">,</span> <span class="st">&quot;{}&quot;</span><span class="op">,</span> <span class="kw">self</span><span class="op">.</span>value)</span>
<span id="cb16-6"><a href="#cb16-6" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb16-7"><a href="#cb16-7" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h2 data-number="6.3" id="fromstr"><span
class="header-section-number">6.3</span> FromStr</h2>
<p><a href="https://doc.rust-lang.org/std/str/trait.FromStr.html"
class="uri">https://doc.rust-lang.org/std/str/trait.FromStr.html</a></p>
<div class="sourceCode" id="cb17"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb17-1"><a href="#cb17-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::</span><span class="op">{</span>num<span class="op">,</span> <span class="dt">str</span><span class="pp">::</span><span class="bu">FromStr</span><span class="op">};</span></span>
<span id="cb17-2"><a href="#cb17-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb17-3"><a href="#cb17-3" aria-hidden="true" tabindex="-1"></a><span class="kw">impl</span> <span class="bu">FromStr</span> <span class="cf">for</span> MyType <span class="op">{</span></span>
<span id="cb17-4"><a href="#cb17-4" aria-hidden="true" tabindex="-1"></a>    <span class="kw">type</span> Err <span class="op">=</span> <span class="pp">num::</span>ParseIntError<span class="op">;</span></span>
<span id="cb17-5"><a href="#cb17-5" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb17-6"><a href="#cb17-6" aria-hidden="true" tabindex="-1"></a>    <span class="kw">fn</span> from_str(s<span class="op">:</span> <span class="op">&amp;</span><span class="dt">str</span>) <span class="op">-&gt;</span> <span class="dt">Result</span><span class="op">&lt;</span><span class="dt">Self</span><span class="op">,</span> <span class="dt">Self</span><span class="pp">::</span><span class="cn">Err</span><span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb17-7"><a href="#cb17-7" aria-hidden="true" tabindex="-1"></a>        <span class="kw">let</span> value <span class="op">=</span> s<span class="op">.</span><span class="pp">parse::</span><span class="op">&lt;</span><span class="dt">usize</span><span class="op">&gt;</span>()<span class="op">?;</span></span>
<span id="cb17-8"><a href="#cb17-8" aria-hidden="true" tabindex="-1"></a>        <span class="cn">Ok</span>(MyType <span class="op">{</span> value <span class="op">}</span>)</span>
<span id="cb17-9"><a href="#cb17-9" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb17-10"><a href="#cb17-10" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h1 data-number="7" id="sync-io"><span
class="header-section-number">7</span> Sync I/O</h1>
<h2 data-number="7.1" id="running-external-commands"><span
class="header-section-number">7.1</span> Running external commands</h2>
<p><a href="https://doc.rust-lang.org/std/process/struct.Command.html"
class="uri">https://doc.rust-lang.org/std/process/struct.Command.html</a></p>
<p><a href="https://doc.rust-lang.org/std/process/struct.Child.html"
class="uri">https://doc.rust-lang.org/std/process/struct.Child.html</a></p>
<p>Run the command and get the whole output directly:</p>
<div class="sourceCode" id="cb18"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb18-1"><a href="#cb18-1" aria-hidden="true" tabindex="-1"></a><span class="pp">Command::</span>new(<span class="st">&quot;ls&quot;</span>)</span>
<span id="cb18-2"><a href="#cb18-2" aria-hidden="true" tabindex="-1"></a>    <span class="op">.</span>arg(<span class="st">&quot;-l&quot;</span>)</span>
<span id="cb18-3"><a href="#cb18-3" aria-hidden="true" tabindex="-1"></a>    <span class="op">.</span>arg(<span class="st">&quot;/&quot;</span>)</span>
<span id="cb18-4"><a href="#cb18-4" aria-hidden="true" tabindex="-1"></a>    <span class="op">.</span>output()<span class="op">?;</span></span></code></pre></div>
<p>Spawn a child, read output line-by-line:</p>
<div class="sourceCode" id="cb19"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb19-1"><a href="#cb19-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::io::</span><span class="op">{</span><span class="bu">BufRead</span><span class="op">,</span> BufReader<span class="op">,</span> <span class="bu">Error</span><span class="op">,</span> ErrorKind<span class="op">};</span></span>
<span id="cb19-2"><a href="#cb19-2" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::process::</span><span class="op">{</span>Command<span class="op">,</span> Stdio<span class="op">};</span></span>
<span id="cb19-3"><a href="#cb19-3" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb19-4"><a href="#cb19-4" aria-hidden="true" tabindex="-1"></a><span class="kw">pub</span> <span class="kw">fn</span> run_commands() <span class="op">-&gt;</span> <span class="pp">std::io::</span><span class="dt">Result</span><span class="op">&lt;</span>()<span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb19-5"><a href="#cb19-5" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> <span class="kw">mut</span> child <span class="op">=</span> <span class="pp">Command::</span>new(<span class="st">&quot;ls&quot;</span>)</span>
<span id="cb19-6"><a href="#cb19-6" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>args([<span class="st">&quot;-l&quot;</span><span class="op">,</span> <span class="st">&quot;/&quot;</span>])</span>
<span id="cb19-7"><a href="#cb19-7" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>stdout(<span class="pp">Stdio::</span>piped())</span>
<span id="cb19-8"><a href="#cb19-8" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>spawn()<span class="op">?;</span></span>
<span id="cb19-9"><a href="#cb19-9" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> stdout <span class="op">=</span> child</span>
<span id="cb19-10"><a href="#cb19-10" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>stdout</span>
<span id="cb19-11"><a href="#cb19-11" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>as_mut()</span>
<span id="cb19-12"><a href="#cb19-12" aria-hidden="true" tabindex="-1"></a>        <span class="op">.</span>ok_or(<span class="bu">Error</span><span class="pp">::</span>from(<span class="pp">ErrorKind::</span>BrokenPipe))<span class="op">?;</span></span>
<span id="cb19-13"><a href="#cb19-13" aria-hidden="true" tabindex="-1"></a>    <span class="cf">for</span> line <span class="kw">in</span> <span class="pp">BufReader::</span>new(stdout)<span class="op">.</span>lines() <span class="op">{</span></span>
<span id="cb19-14"><a href="#cb19-14" aria-hidden="true" tabindex="-1"></a>        <span class="pp">println!</span>(<span class="st">&quot;{:?}&quot;</span><span class="op">,</span> line)<span class="op">;</span></span>
<span id="cb19-15"><a href="#cb19-15" aria-hidden="true" tabindex="-1"></a>    <span class="op">}</span></span>
<span id="cb19-16"><a href="#cb19-16" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> result <span class="op">=</span> child<span class="op">.</span>wait()<span class="op">?;</span></span>
<span id="cb19-17"><a href="#cb19-17" aria-hidden="true" tabindex="-1"></a>    <span class="pp">println!</span>(<span class="st">&quot;{:?}&quot;</span><span class="op">,</span> result)<span class="op">;</span></span>
<span id="cb19-18"><a href="#cb19-18" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(())</span>
<span id="cb19-19"><a href="#cb19-19" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h1 data-number="8" id="async-io-with-tokio"><span
class="header-section-number">8</span> Async I/O with
<code>tokio</code></h1>
<h2 data-number="8.1" id="running-external-commands-1"><span
class="header-section-number">8.1</span> Running external commands</h2>
<p><a href="https://docs.rs/tokio/latest/tokio/process/"
class="uri">https://docs.rs/tokio/latest/tokio/process/</a></p>
<pre><code>cargo add tokio --features=macros,rt,rt-multi-thread,process</code></pre>
<div class="sourceCode" id="cb21"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb21-1"><a href="#cb21-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">tokio::process::</span>Command<span class="op">;</span></span>
<span id="cb21-2"><a href="#cb21-2" aria-hidden="true" tabindex="-1"></a></span>
<span id="cb21-3"><a href="#cb21-3" aria-hidden="true" tabindex="-1"></a><span class="kw">pub</span> <span class="kw">async</span> <span class="kw">fn</span> run_commands() <span class="op">-&gt;</span> <span class="pp">std::io::</span><span class="dt">Result</span><span class="op">&lt;</span>()<span class="op">&gt;</span> <span class="op">{</span></span>
<span id="cb21-4"><a href="#cb21-4" aria-hidden="true" tabindex="-1"></a>    <span class="kw">let</span> output <span class="op">=</span> <span class="pp">Command::</span>new(<span class="st">&quot;ls&quot;</span>)<span class="op">.</span>args([<span class="st">&quot;-l&quot;</span><span class="op">,</span> <span class="st">&quot;/&quot;</span>])<span class="op">.</span>output()<span class="op">.</span><span class="kw">await</span><span class="op">?;</span></span>
<span id="cb21-5"><a href="#cb21-5" aria-hidden="true" tabindex="-1"></a>    <span class="pp">println!</span>(<span class="st">&quot;{:?}&quot;</span><span class="op">,</span> output<span class="op">.</span>status)<span class="op">;</span></span>
<span id="cb21-6"><a href="#cb21-6" aria-hidden="true" tabindex="-1"></a>    <span class="pp">println!</span>(<span class="st">&quot;{:?}&quot;</span><span class="op">,</span> output<span class="op">.</span>stdout)<span class="op">;</span></span>
<span id="cb21-7"><a href="#cb21-7" aria-hidden="true" tabindex="-1"></a>    <span class="cn">Ok</span>(())</span>
<span id="cb21-8"><a href="#cb21-8" aria-hidden="true" tabindex="-1"></a><span class="op">}</span></span></code></pre></div>
<h1 data-number="9" id="misc-snippets-i.e.-unclassified"><span
class="header-section-number">9</span> Misc snippets
(i.e. unclassified)</h1>
<h2 data-number="9.1" id="entry"><span
class="header-section-number">9.1</span> Entry</h2>
<p><a
href="https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html"
class="uri">https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html</a></p>
<p>Or “how to efficiently update a collection entry that may not be
there”.</p>
<div class="sourceCode" id="cb22"><pre
class="sourceCode rust"><code class="sourceCode rust"><span id="cb22-1"><a href="#cb22-1" aria-hidden="true" tabindex="-1"></a><span class="kw">use</span> <span class="pp">std::collections::</span>HashMap<span class="op">;</span></span>
<span id="cb22-2"><a href="#cb22-2" aria-hidden="true" tabindex="-1"></a><span class="kw">let</span> <span class="kw">mut</span> hashmap<span class="op">:</span> HashMap<span class="op">&lt;</span><span class="dt">i32</span><span class="op">,</span> <span class="dt">Vec</span><span class="op">&lt;</span><span class="dt">i32</span><span class="op">&gt;&gt;</span> <span class="op">=</span> <span class="pp">HashMap::</span>new()<span class="op">;</span></span>
<span id="cb22-3"><a href="#cb22-3" aria-hidden="true" tabindex="-1"></a><span class="kw">let</span> key <span class="op">=</span> <span class="dv">5</span><span class="op">;</span></span>
<span id="cb22-4"><a href="#cb22-4" aria-hidden="true" tabindex="-1"></a><span class="co">// The or_* methods return &amp;mut v, which means we can also::</span></span>
<span id="cb22-5"><a href="#cb22-5" aria-hidden="true" tabindex="-1"></a><span class="kw">let</span> entry <span class="op">=</span> hashmap<span class="op">.</span>entry(key)<span class="op">.</span>or_default()<span class="op">;</span></span>
<span id="cb22-6"><a href="#cb22-6" aria-hidden="true" tabindex="-1"></a>entry<span class="op">.</span>push(<span class="dv">9</span>)<span class="op">;</span></span>
<span id="cb22-7"><a href="#cb22-7" aria-hidden="true" tabindex="-1"></a><span class="co">// These 3 options also do the same:</span></span>
<span id="cb22-8"><a href="#cb22-8" aria-hidden="true" tabindex="-1"></a>hashmap<span class="op">.</span>entry(key)<span class="op">.</span>or_default()<span class="op">.</span>push(<span class="dv">9</span>)<span class="op">;</span></span>
<span id="cb22-9"><a href="#cb22-9" aria-hidden="true" tabindex="-1"></a>hashmap<span class="op">.</span>entry(key)<span class="op">.</span>or_insert(<span class="pp">vec!</span>[])<span class="op">.</span>push(<span class="dv">9</span>)<span class="op">;</span></span>
<span id="cb22-10"><a href="#cb22-10" aria-hidden="true" tabindex="-1"></a>hashmap<span class="op">.</span>entry(key)<span class="op">.</span>or_insert_with(<span class="op">||</span> <span class="pp">vec!</span>[])<span class="op">.</span>push(<span class="dv">9</span>)<span class="op">;</span></span></code></pre></div>
