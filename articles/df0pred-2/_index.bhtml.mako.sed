<div id="toc" style="margin-bottom: 2em;">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#the-problem-with-the-linear-regression"
id="toc-the-problem-with-the-linear-regression"><span
class="toc-section-number">1</span> The problem with the linear
regression</a></li>
<li><a href="#a-naïve-new-method-averaging-the-difference"
id="toc-a-naïve-new-method-averaging-the-difference"><span
class="toc-section-number">2</span> A naïve new method: averaging the
difference</a></li>
<li><a href="#the-real-new-method-days-left-by-monte-carlo-simulation"
id="toc-the-real-new-method-days-left-by-monte-carlo-simulation"><span
class="toc-section-number">3</span> The real new method: days left by
Monte Carlo simulation</a></li>
<li><a href="#conclusion" id="toc-conclusion"><span
class="toc-section-number">4</span> Conclusion</a></li>
<li><a href="#further-reading" id="toc-further-reading"><span
class="toc-section-number">5</span> Further reading</a></li>
</ul>
</div>
<p>On the <a href="$cwd$/../df0pred-1/index.html">first</a> article, we
saw a quick-and-dirty method to predict disk space exhaustion when the
usage pattern is rigorously linear. We did that by importing our data
into <a
href="https://en.wikipedia.org/wiki/R_programming_language">R</a> and
making a linear regression.</p>
<p>In this article we will see the problems with that method, and deploy
a more robust solution. Besides robustness, we will also see how we can
generate a probability distribution for the date of disk space
exhaustion instead of calculating a single day.</p>
<h1 data-number="1" id="the-problem-with-the-linear-regression"><span
class="header-section-number">1</span> The problem with the linear
regression</h1>
<p>The linear regression used in the first article has a serious lack of
<a
href="https://en.wikipedia.org/wiki/Robust_statistics">robustness</a>.
That means that it is very sensitive to even single departures from the
linear pattern. For instance, if we periodically delete some big files
in the hard disk, we end up breaking the sample in parts that cannot be
analysed together. If we plot the line given by the linear model, we can
see clearly that it does not fit our overall data very well:</p>
<p><img src="$cwd$/lm.png" /></p>
<p>(<a href="$cwd$/duinfospike.dat">Data file</a>)</p>
<p>We can see in the graph that the linear model gives us a line that
our free disk space is increasing instead of decreasing! If we use this
model, we will reach the conclusion that we will never reach df0.</p>
<p>If we keep analysing used disk space, there is not much we can do
besides discarding the data gathered before the last cleanup. There is
no way to easily ignore only the cleanup.</p>
<p>In fact, we can only use the linear regression method when our disk
consumption pattern is linear for the analysed period - and that rarely
is the case when there is human intervention. We should always look at
the graph to see if the model makes sense.</p>
<h1 data-number="2"
id="a-naïve-new-method-averaging-the-difference"><span
class="header-section-number">2</span> A naïve new method: averaging the
difference</h1>
<p>Instead of using the daily used disk space as input, we will use the
daily <strong>difference</strong> (or delta) of used disk space. By
itself, this reduces a big disk cleanup to a single outlier instead of
breaking our sample. We could then just filter out the outliers,
calculate the average daily increment in used disk space and divide the
current free space by it. That would give us the average number of days
left until disk exhaustion. Well, that would also give us some new
problems to solve.</p>
<p>The first problem is that filtering out the outliers is neither
straightforward nor recommended. Afterall, we are throwing out data that
might be meaningful: it could be a regular monthly process that we
should take into account to generate a better prediction.</p>
<p>Besides, by averaging disk consumption and dividing free disk space
by it, we would still not have the probability distribution for the
date, only a single value.</p>
<h1 data-number="3"
id="the-real-new-method-days-left-by-monte-carlo-simulation"><span
class="header-section-number">3</span> The real new method: days left by
Monte Carlo simulation</h1>
<p>Instead of calculating the number of days left from the data, we will
use a technique called <a
href="https://en.wikipedia.org/wiki/Monte_carlo_simulation">Monte Carlo
simulation</a> to generate the distribution of days left. The idea is
simple: we sample the data we have - daily used disk space - until the
sum is above the free disk space; the number of samples taken is the
number of days left. By doing that repeatedly, we get the set of
“possible days left” with a distribution that corresponds to the data we
have collected. Let’s how we can do that in R.</p>
<p>First, let’s load the data file that we will use (same one used in
the introduction) along with a variable that holds the size of the disk
(500GB; all units are in MB):</p>
<pre><code>duinfo &lt;- read.table(&#39;duinfospike.dat&#39;,
        colClasses=c(&quot;Date&quot;,&quot;numeric&quot;),
        col.names=c(&quot;day&quot;,&quot;usd&quot;))
attach(duinfo)
totalspace &lt;- 500000
today &lt;- tail(day, 1)</code></pre>
<p>We now get the delta of the disk usage. Let’s take a look at it:</p>
<pre><code>dudelta &lt;- diff(usd)</code></pre>
<pre><code>plot(dudelta, xaxt=&#39;n&#39;, xlab=&#39;&#39;)</code></pre>
<p><img src="$cwd$/delta.png" /></p>
<p>The summary function gives us the five-number summary, while the
boxplot shows us how the data is distributed graphically:</p>
<pre><code>summary(dudelta)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-29583.00      5.25    301.00    123.37    713.00   4136.00 </code></pre>
<pre><code>boxplot(dudelta)</code></pre>
<p><img src="$cwd$/deltabox.png" /></p>
<p>The kernel density plot gives us about the same, but in another
visual format:</p>
<pre><code>plot(density(dudelta))</code></pre>
<p><img src="$cwd$/deltakd.png" /></p>
<p>We can see the cleanups right there, as the lower points.</p>
<p>The next step is the creation of the sample of the number of days
left until exhaustion. In order to do that, we create an R function that
sums values taken randomly from our delta sample until our free space
zeroes, and returns the number of samples taken:</p>
<pre><code>f &lt;- function(spaceleft) {
    days &lt;- 0
    while(spaceleft &gt; 0) {
        days &lt;- days + 1
        spaceleft &lt;- spaceleft - sample(dudelta, 1, replace=TRUE)
    }
    days
}</code></pre>
<p>By repeatedly running this function and gathering the results, we
generate a set of number-of-days-until-exhaustion that is robust and
corresponds to the data we have observed. This robustness means that we
don’t even need to remove outliers, as they will not disproportionally
bias out results:</p>
<pre><code>freespace &lt;- totalspace - tail(usd, 1)
daysleft &lt;- replicate(5000, f(freespace))</code></pre>
<pre><code>plot(daysleft)</code></pre>
<p><img src="$cwd$/daysleft.png" /></p>
<p>What we want now is the <a
href="https://en.wikipedia.org/wiki/Empirical_distribution_function">empirical
cumulative distribution</a>. This function gives us the probability that
we will reach df0 <strong>before</strong> the given date.</p>
<pre><code>df0day &lt;- sort(daysleft + today)
df0ecdfunc &lt;- ecdf(df0day)
df0prob &lt;- df0ecdfunc(df0day)</code></pre>
<pre><code>plot(df0day, df0prob, xaxt=&#39;n&#39;, type=&#39;l&#39;)
axis.Date(1, df0day, at=seq(min(df0day), max(df0day), &#39;year&#39;), format=&#39;%F&#39;)</code></pre>
<p><img src="$cwd$/df0ecdf.png" /></p>
<p>With the cumulative probability estimate, we can see when we have to
start worrying about the disk by looking at the first day that the
probability of df0 is above 0:</p>
<pre><code>df0day[1]
[1] &quot;2010-06-13&quot;
df0ecdfunc(df0day[1])
[1] 2e-04</code></pre>
<p>Well, we can also be a bit more bold and wait until the chances of
reaching df0 rise above 5%:</p>
<pre><code>df0day[which(df0prob &gt; 0.05)[1]]
[1] &quot;2010-08-16&quot;</code></pre>
<p>Mix and match and see what a good convention for your case is.</p>
<h1 data-number="4" id="conclusion"><span
class="header-section-number">4</span> Conclusion</h1>
<p>This and the <a href="$cwd$/../df0pred-1/index.html">previous
article</a> showed how to use statistics in R to predict when free
hard-disk space will zero.</p>
<p>The first article was main purpose was to serve as an introduction to
R. There are many reasons that make linear regression an unsuitable
technique for df0 prediction - the underlying process of disk
consumption is certainly not linear. But, if the graph shows you that
the line fits, there is no reason to ignore it.</p>
<p>Monte Carlo simulation, on the other hand, is a powerful and general
technique. It assumes little about the data (non-parameterized), and it
can give you probability distributions. If you want to forecast
something, you can always start recording data and use Monte Carlo in
some way to make predictions <strong>based on the evidence</strong>.
Personally, I think we don’t do this nearly as often as we could. Well,
<a href="http://www.joelonsoftware.com/items/2007/10/26.html">Joel is
even using it to make schedules</a>.</p>
<h1 data-number="5" id="further-reading"><span
class="header-section-number">5</span> Further reading</h1>
<ul>
<li><a href="http://www.joelonsoftware.com/items/2007/10/26.html"
class="uri">http://www.joelonsoftware.com/items/2007/10/26.html</a>:
Joel’s use of Monte Carlo to make schedules.</li>
<li><a
href="https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29"
class="uri">https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29</a>:
Wikipedia’s page on bootstrapping, which is clearer than the one on
Monte Carlo simulations.</li>
<li><a href="http://www.r-bloggers.com/"
class="uri">http://www.r-bloggers.com/</a>: daily news and tutorials
about R, very good to learn the language and see what people are doing
with it.</li>
</ul>
