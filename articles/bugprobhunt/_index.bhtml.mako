<div id="toc" style="margin-bottom: 2em;">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#thebug" id="toc-thebug"><span
class="toc-section-number">1</span> The Bug</a></li>
<li><a href="#the-model" id="toc-the-model"><span
class="toc-section-number">2</span> The Model</a>
<ul>
<li><a href="#physical-analogy" id="toc-physical-analogy"><span
class="toc-section-number">2.1</span> Physical analogy</a></li>
</ul></li>
<li><a href="#estimating-p" id="toc-estimating-p"><span
class="toc-section-number">3</span> Estimating <span
class="math inline">\(p\)</span></a>
<ul>
<li><a href="#p-precision" id="toc-p-precision"><span
class="toc-section-number">3.1</span> <span
class="math inline">\(p\)</span> precision</a></li>
<li><a href="#p-likelihood" id="toc-p-likelihood"><span
class="toc-section-number">3.2</span> <span
class="math inline">\(p\)</span> likelihood</a></li>
<li><a href="#increasing-n-narrowing-down-the-interval"
id="toc-increasing-n-narrowing-down-the-interval"><span
class="toc-section-number">3.3</span> Increasing <span
class="math inline">\(n\)</span>, narrowing down the interval</a></li>
<li><a href="#investigation-framework"
id="toc-investigation-framework"><span
class="toc-section-number">3.4</span> Investigation framework</a></li>
</ul></li>
<li><a href="#is-the-bug-fixed" id="toc-is-the-bug-fixed"><span
class="toc-section-number">4</span> Is the bug fixed?</a></li>
<li><a href="#back-to-the-bug" id="toc-back-to-the-bug"><span
class="toc-section-number">5</span> Back to the Bug</a>
<ul>
<li><a href="#testing-20-times" id="toc-testing-20-times"><span
class="toc-section-number">5.1</span> Testing 20 times</a></li>
<li><a href="#testing-5000-times" id="toc-testing-5000-times"><span
class="toc-section-number">5.2</span> Testing 5000 times</a></li>
</ul></li>
<li><a href="#optimal-testing" id="toc-optimal-testing"><span
class="toc-section-number">6</span> Optimal testing</a></li>
<li><a href="#conclusions" id="toc-conclusions"><span
class="toc-section-number">7</span> Conclusions</a></li>
</ul>
</div>
<p>Have you ever run into a bug that, no matter how careful you are
trying to reproduce it, it only happens sometimes? And then, you think
you’ve got it, and finally solved it - and tested a couple of times
without any manifestation. How do you know that you have tested enough?
Are you sure you were not “lucky” in your tests?</p>
<p>In this article we will see how to answer those questions and the
math behind it without going into too much detail. This is a pragmatic
guide.</p>
<h1 data-number="1" id="thebug"><span
class="header-section-number">1</span> The Bug</h1>
<p>The following program is supposed to generate two random 8-bit
integer and print them on stdout:</p>
<pre><code>#include &lt;stdio.h&gt;
#include &lt;fcntl.h&gt;
#include &lt;unistd.h&gt;

/* Returns -1 if error, other number if ok. */
int get_random_chars(char *r1, char*r2)
{
    int f = open(&quot;/dev/urandom&quot;, O_RDONLY);

    if (f &lt; 0)
        return -1;
    if (read(f, r1, sizeof(*r1)) &lt; 0)
        return -1;
    if (read(f, r2, sizeof(*r2)) &lt; 0)
        return -1;
    close(f);

    return *r1 &amp; *r2;
}

int main(void)
{
    char r1;
    char r2;
    int ret;

    ret = get_random_chars(&amp;r1, &amp;r2);

    if (ret &lt; 0)
        fprintf(stderr, &quot;error&quot;);
    else
        printf(&quot;%d %d\n&quot;, r1, r2);

    return ret &lt; 0;
}</code></pre>
<p>On my architecture (Linux on IA-32) it has a bug that makes it print
“error” instead of the numbers sometimes.</p>
<h1 data-number="2" id="the-model"><span
class="header-section-number">2</span> The Model</h1>
<p>Every time we run the program, the bug can either show up or not. It
has a non-deterministic behaviour that requires statistical
analysis.</p>
<p>We will model a single program run as a <a
href="https://en.wikipedia.org/wiki/Bernoulli_trial">Bernoulli
trial</a>, with success defined as “seeing the bug”, as that is the
event we are interested in. We have the following parameters when using
this model:</p>
<ul>
<li><span class="math inline">\(n\)</span>: the number of tests
made;</li>
<li><span class="math inline">\(k\)</span>: the number of times the bug
was observed in the <span class="math inline">\(n\)</span> tests;</li>
<li><span class="math inline">\(p\)</span>: the unknown (and, most of
the time, unknowable) probability of seeing the bug.</li>
</ul>
<p>As a Bernoulli trial, the number of errors <span
class="math inline">\(k\)</span> of running the program <span
class="math inline">\(n\)</span> times follows a <a
href="https://en.wikipedia.org/wiki/Binomial_distribution">binomial
distribution</a> <span class="math inline">\(k \sim B(n,p)\)</span>. We
will use this model to estimate <span class="math inline">\(p\)</span>
and to confirm the hypotheses that the bug no longer exists, after
fixing the bug in whichever way we can.</p>
<p>By using this model we are implicitly assuming that all our tests are
performed independently and identically. In order words: if the bug
happens more ofter in one environment, we either test always in that
environment or never; if the bug gets more and more frequent the longer
the computer is running, we reset the computer after each trial. If we
don’t do that, we are effectively estimating the value of <span
class="math inline">\(p\)</span> with trials from different experiments,
while in truth each experiment has its own <span
class="math inline">\(p\)</span>. We will find a single value anyway,
but it has no meaning and can lead us to wrong conclusions.</p>
<h2 data-number="2.1" id="physical-analogy"><span
class="header-section-number">2.1</span> Physical analogy</h2>
<p>Another way of thinking about the model and the strategy is by
creating a physical analogy with a box that has an unknown number of
green and red balls:</p>
<ul>
<li>Bernoulli trial: taking a single ball out of the box and looking at
its color - if it is red, we have observed the bug, otherwise we
haven’t. We then put the ball back in the box.</li>
<li><span class="math inline">\(n\)</span>: the total number of trials
we have performed.</li>
<li><span class="math inline">\(k\)</span>: the total number of red
balls seen.</li>
<li><span class="math inline">\(p\)</span>: the total number of red
balls in the box divided by the total number of green balls in the
box.</li>
</ul>
<p>Some things become clearer when we think about this analogy:</p>
<ul>
<li>If we open the box and count the balls, we can know <span
class="math inline">\(p\)</span>, in contrast with our original
problem.</li>
<li>Without opening the box, we can estimate <span
class="math inline">\(p\)</span> by repeating the trial. As <span
class="math inline">\(n\)</span> increases, our estimate for <span
class="math inline">\(p\)</span> improves. Mathematically: <span
class="math display">\[p = \lim_{n\to\infty}\frac{k}{n}\]</span></li>
<li>Performing the trials in different conditions is like taking balls
out of several different boxes. The results tell us nothing about any
single box.</li>
</ul>
<p><img src="$cwd$/boxballs.png" /></p>
<h1 data-number="3" id="estimating-p"><span
class="header-section-number">3</span> Estimating <span
class="math inline">\(p\)</span></h1>
<p>Before we try fixing anything, we have to know more about the bug,
starting by the probability <span class="math inline">\(p\)</span> of
reproducing it. We can estimate this probability by dividing the number
of times we see the bug <span class="math inline">\(k\)</span> by the
number of times we tested for it <span class="math inline">\(n\)</span>.
Let’s try that with our sample bug:</p>
<pre><code>$ ./hasbug
67 -68
$ ./hasbug
79 -101
$ ./hasbug
error</code></pre>
<p>We know from the source code that <span
class="math inline">\(p=25\%\)</span>, but let’s pretend that we don’t,
as will be the case with practically every non-deterministic bug. We
tested 3 times, so <span class="math inline">\(k=1, n=3 \Rightarrow p
\sim 33\%\)</span>, right? It would be better if we tested more, but how
much more, and exactly what would be better?</p>
<h2 data-number="3.1" id="p-precision"><span
class="header-section-number">3.1</span> <span
class="math inline">\(p\)</span> precision</h2>
<p>Let’s go back to our box analogy: imagine that there are 4 balls in
the box, one red and three green. That means that <span
class="math inline">\(p = 1/4\)</span>. What are the possible results
when we test three times?</p>
<table class="table table-bordered">
<thead>
<tr class="header">
<th>Red balls</th>
<th>Green balls</th>
<th><span class="math inline">\(p\)</span> estimate</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>0</td>
<td>3</td>
<td>0%</td>
</tr>
<tr class="even">
<td>1</td>
<td>2</td>
<td>33%</td>
</tr>
<tr class="odd">
<td>2</td>
<td>1</td>
<td>66%</td>
</tr>
<tr class="even">
<td>3</td>
<td>0</td>
<td>100%</td>
</tr>
</tbody>
</table>
<p>The less we test, the smaller our precision is. Roughly, <span
class="math inline">\(p\)</span> precision will be at most <span
class="math inline">\(1/n\)</span> - in this case, 33%. That’s the step
of values we can find for <span class="math inline">\(p\)</span>, and
the minimal value for it.</p>
<p>Testing more improves the precision of our estimate.</p>
<h2 data-number="3.2" id="p-likelihood"><span
class="header-section-number">3.2</span> <span
class="math inline">\(p\)</span> likelihood</h2>
<p>Let’s now approach the problem from another angle: if <span
class="math inline">\(p = 1/4\)</span>, what are the odds of seeing one
error in four tests? Let’s name the 4 balls as 0-red, 1-green, 2-green
and 3-green:</p>
<iframe src="r1w3_n4_results.html" style="width:100%;height:500px;">
</iframe>
<p>The table above has all the possible results for getting 4 balls out
of the box. That’s <span class="math inline">\(4^4=256\)</span> rows,
generated by <a href="$cwd$/box">this</a> python script. The same script
counts the number of red balls in each row, and outputs the following
table:</p>
<table class="table table-bordered">
<thead>
<tr class="header">
<th>k</th>
<th>rows</th>
<th>%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>4</td>
<td>1</td>
<td>0.39%</td>
</tr>
<tr class="even">
<td>3</td>
<td>12</td>
<td>4.69%</td>
</tr>
<tr class="odd">
<td>2</td>
<td>54</td>
<td>21.09%</td>
</tr>
<tr class="even">
<td>1</td>
<td>108</td>
<td>42.19%</td>
</tr>
<tr class="odd">
<td>0</td>
<td>81</td>
<td>31.64%</td>
</tr>
</tbody>
</table>
<p>That means that, for <span class="math inline">\(p=1/4\)</span>, we
see 1 red ball and 3 green balls only 42% of the time when getting out 4
balls.</p>
<p>What if <span class="math inline">\(p = 1/3\)</span> - one red ball
and two green balls? We would get the following table:</p>
<table class="table table-bordered">
<thead>
<tr class="header">
<th>k</th>
<th>rows</th>
<th>%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>4</td>
<td>1</td>
<td>1.23%</td>
</tr>
<tr class="even">
<td>3</td>
<td>8</td>
<td>9.88%</td>
</tr>
<tr class="odd">
<td>2</td>
<td>24</td>
<td>29.63%</td>
</tr>
<tr class="even">
<td>1</td>
<td>32</td>
<td>39.51%</td>
</tr>
<tr class="odd">
<td>0</td>
<td>16</td>
<td>19.75%</td>
</tr>
</tbody>
</table>
<p>What about <span class="math inline">\(p = 1/2\)</span>?</p>
<table class="table table-bordered">
<thead>
<tr class="header">
<th>k</th>
<th>rows</th>
<th>%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>4</td>
<td>1</td>
<td>6.25%</td>
</tr>
<tr class="even">
<td>3</td>
<td>4</td>
<td>25.00%</td>
</tr>
<tr class="odd">
<td>2</td>
<td>6</td>
<td>37.50%</td>
</tr>
<tr class="even">
<td>1</td>
<td>4</td>
<td>25.00%</td>
</tr>
<tr class="odd">
<td>0</td>
<td>1</td>
<td>6.25%</td>
</tr>
</tbody>
</table>
<p>So, let’s assume that you’ve seen the bug once in 4 trials. What is
the value of <span class="math inline">\(p\)</span>? You know that can
happen 42% of the time if <span class="math inline">\(p=1/4\)</span>,
but you also know it can happen 39% of the time if <span
class="math inline">\(p=1/3\)</span>, and 25% of the time if <span
class="math inline">\(p=1/2\)</span>. Which one is it?</p>
<p>The graph bellow shows the discrete likelihood for all <span
class="math inline">\(p\)</span> percentual values for getting 1 red and
3 green balls:</p>
<p><img src="$cwd$/r1w3_dist.png" /></p>
<p>The fact is that, <em>given the data</em>, the estimate for <span
class="math inline">\(p\)</span> follows a <a
href="https://en.wikipedia.org/wiki/Beta_distribution">beta
distribution</a> <span class="math inline">\(Beta(k+1, n-k+1) = Beta(2,
4)\)</span> (<a
href="http://stats.stackexchange.com/questions/13225/what-is-the-distribution-of-the-binomial-distribution-parameter-p-given-a-samp">1</a>)
The graph below shows the probability distribution density of <span
class="math inline">\(p\)</span>:</p>
<p><img src="$cwd$/r1w3_dens.png" /></p>
<p>The R script used to generate the first plot is <a
href="$cwd$/pdistplot.R">here</a>, the one used for the second plot is
<a href="$cwd$/pdensplot.R">here</a>.</p>
<h2 data-number="3.3"
id="increasing-n-narrowing-down-the-interval"><span
class="header-section-number">3.3</span> Increasing <span
class="math inline">\(n\)</span>, narrowing down the interval</h2>
<p>What happens when we test more? We obviously increase our precision,
as it is at most <span class="math inline">\(1/n\)</span>, as we said
before - there is no way to estimate that <span
class="math inline">\(p=1/3\)</span> when we only test twice. But there
is also another effect: the distribution for <span
class="math inline">\(p\)</span> gets taller and narrower around the
observed ratio <span class="math inline">\(k/n\)</span>:</p>
<p><img src="$cwd$/pdens_many.png" /></p>
<h2 data-number="3.4" id="investigation-framework"><span
class="header-section-number">3.4</span> Investigation framework</h2>
<p>So, which value will we use for <span
class="math inline">\(p\)</span>?</p>
<ul>
<li><p>The smaller the value of <span class="math inline">\(p\)</span>,
the more we have to test to reach a given confidence in the bug
solution.</p></li>
<li><p>We must, then, choose the probability of error that we want to
tolerate, and take the <em>smallest</em> value of <span
class="math inline">\(p\)</span> that we can.</p>
<p>A usual value for the probability of error is 5% (2.5% on each
side).</p></li>
<li><p>That means that we take the value of <span
class="math inline">\(p\)</span> that leaves 2.5% of the area of the
density curve out on the left side. Let’s call this value <span
class="math inline">\(p_{min}\)</span>.</p></li>
<li><p>That way, if the observed <span
class="math inline">\(k/n\)</span> remains somewhat constant, <span
class="math inline">\(p_{min}\)</span> will raise, converging to the
“real” <span class="math inline">\(p\)</span> value.</p></li>
<li><p>As <span class="math inline">\(p_{min}\)</span> raises, the
amount of testing we have to do after fixing the bug decreases.</p></li>
</ul>
<p>By using this framework we have direct, visual and tangible
incentives to test more. We can objectively measure the potential
contribution of each test.</p>
<p>In order to calculate <span class="math inline">\(p_{min}\)</span>
with the mentioned properties, we have to solve the following
equation:</p>
<p><span class="math display">\[\sum_{k=0}^{k}{n\choose{k}}p_{min}
^k(1-p_{min})^{n-k}=\frac{\alpha}{2} \]</span></p>
<p><span class="math inline">\(alpha\)</span> here is twice the error we
want to tolerate: 5% for an error of 2.5%.</p>
<p>That’s not a trivial equation to solve for <span
class="math inline">\(p_{min}\)</span>. Fortunately, that’s the formula
for the confidence interval of the binomial distribution, and there are
a lot of sites that can calculate it:</p>
<ul>
<li><a href="http://statpages.info/confint.html"
class="uri">http://statpages.info/confint.html</a>: <span
class="math inline">\(\alpha\)</span> here is 5%.</li>
<li><a href="http://www.danielsoper.com/statcalc3/calc.aspx?id=85"
class="uri">http://www.danielsoper.com/statcalc3/calc.aspx?id=85</a>:
results for <span class="math inline">\(\alpha\)</span> 1%, 5% and
10%.</li>
<li><a
href="https://www.google.com.br/search?q=binomial+confidence+interval+calculator"
class="uri">https://www.google.com.br/search?q=binomial+confidence+interval+calculator</a>:
google search.</li>
</ul>
<h1 data-number="4" id="is-the-bug-fixed"><span
class="header-section-number">4</span> Is the bug fixed?</h1>
<p>So, you have tested a lot and calculated <span
class="math inline">\(p_{min}\)</span>. The next step is fixing the
bug.</p>
<p>After fixing the bug, you will want to test again, in order to
confirm that the bug is fixed. How much testing is enough testing?</p>
<p>Let’s say that <span class="math inline">\(t\)</span> is the number
of times we test the bug after it is fixed. Then, if our fix is not
effective and the bug still presents itself with a probability greater
than the <span class="math inline">\(p_{min}\)</span> that we
calculated, the probability of <em>not</em> seeing the bug after <span
class="math inline">\(t\)</span> tests is:</p>
<p><span class="math display">\[\alpha = (1-p_{min})^t \]</span></p>
<p>Here, <span class="math inline">\(\alpha\)</span> is also the
probability of making a <a
href="https://en.wikipedia.org/wiki/Type_I_and_type_II_errors#Type_I_error">type
I error</a>, while <span class="math inline">\(1 - \alpha\)</span> is
the <em>statistical significance</em> of our tests.</p>
<p>We now have two options:</p>
<ul>
<li>arbitrarily determining a standard statistical significance and
testing enough times to assert it.</li>
<li>test as much as we can and report the achieved statistical
significance.</li>
</ul>
<p>Both options are valid. The first one is not always feasible, as the
cost of each trial can be high in time and/or other kind of
resources.</p>
<p>The standard statistical significance in the industry is 5%, we
recommend either that or less.</p>
<p>Formally, this is very similar to a <a
href="https://en.wikipedia.org/wiki/Hypothesis_testing">statistical
hypothesis testing</a>.</p>
<h1 data-number="5" id="back-to-the-bug"><span
class="header-section-number">5</span> Back to the Bug</h1>
<h2 data-number="5.1" id="testing-20-times"><span
class="header-section-number">5.1</span> Testing 20 times</h2>
<p><a href="$cwd$/trials.csv">This file trials.csv</a> has the results
found after running our program 5000 times. We must never throw out
data, but let’s pretend that we have tested our program only 20 times.
The observed <span class="math inline">\(k/n\)</span> ration and the
calculated <span class="math inline">\(p_{min}\)</span> evolved as shown
in the following graph:</p>
<p><img src="$cwd$/trials20.png" /></p>
<p>After those 20 tests, our <span
class="math inline">\(p_{min}\)</span> is about 12%.</p>
<p>Suppose that we fix the bug and test it again. The following graph
shows the statistical significance corresponding to the number of tests
we do:</p>
<p><img src="$cwd$/after20.png" /></p>
<p>In words: we have to test 24 times after fixing the bug to reach 95%
statistical significance, and 35 to reach 99%.</p>
<p>Now, what happens if we test more before fixing the bug?</p>
<h2 data-number="5.2" id="testing-5000-times"><span
class="header-section-number">5.2</span> Testing 5000 times</h2>
<p>Let’s now use all the results and assume that we tested 5000 times
before fixing the bug. The graph bellow shows <span
class="math inline">\(k/n\)</span> and <span
class="math inline">\(p_{min}\)</span>:</p>
<p><img src="$cwd$/trials5000.png" /></p>
<p>After those 5000 tests, our <span
class="math inline">\(p_{min}\)</span> is about 23% - much closer to the
real <span class="math inline">\(p\)</span>.</p>
<p>The following graph shows the statistical significance corresponding
to the number of tests we do after fixing the bug:</p>
<p><img src="$cwd$/after5000.png" /></p>
<p>We can see in that graph that after about 11 tests we reach 95%, and
after about 16 we get to 99%. As we have tested more before fixing the
bug, we found a higher <span class="math inline">\(p_{min}\)</span>, and
that allowed us to test less after fixing the bug.</p>
<h1 data-number="6" id="optimal-testing"><span
class="header-section-number">6</span> Optimal testing</h1>
<p>We have seen that we decrease <span class="math inline">\(t\)</span>
as we increase <span class="math inline">\(n\)</span>, as that can
potentially increases our lower estimate for <span
class="math inline">\(p\)</span>. Of course, that value can decrease as
we test, but that means that we “got lucky” in the first trials and we
are getting to know the bug better - the estimate is approaching the
real value in a non-deterministic way, after all.</p>
<p>But, how much should we test before fixing the bug? Which value is an
ideal value for <span class="math inline">\(n\)</span>?</p>
<p>To define an optimal value for <span
class="math inline">\(n\)</span>, we will minimize the sum <span
class="math inline">\(n+t\)</span>. This objective gives us the benefit
of minimizing the total amount of testing without compromising our
guarantees. Minimizing the testing can be fundamental if each test costs
significant time and/or resources.</p>
<p>The graph bellow shows us the evolution of the value of <span
class="math inline">\(t\)</span> and <span
class="math inline">\(t+n\)</span> using the data we generated for our
bug:</p>
<p><img src="$cwd$/tbyn.png" /></p>
<p>We can see clearly that there are some low values of <span
class="math inline">\(n\)</span> and <span
class="math inline">\(t\)</span> that give us the guarantees we need.
Those values are <span class="math inline">\(n = 15\)</span> and <span
class="math inline">\(t = 24\)</span>, which gives us <span
class="math inline">\(t+n = 39\)</span>.</p>
<p>While you can use this technique to minimize the total number of
tests performed (even more so when testing is expensive), testing more
is always a good thing, as it always improves our guarantee, be it in
<span class="math inline">\(n\)</span> by providing us with a better
<span class="math inline">\(p\)</span> or in <span
class="math inline">\(t\)</span> by increasing the statistical
significance of the conclusion that the bug is fixed. So, before fixing
the bug, test until you see the bug at least once, and then at least the
amount specified by this technique - but also test more if you can,
there is no upper bound, specially after fixing the bug. You can then
report a higher confidence in the solution.</p>
<h1 data-number="7" id="conclusions"><span
class="header-section-number">7</span> Conclusions</h1>
<p>When a programmer finds a bug that behaves in a non-deterministic
way, he knows he should test enough to know more about the bug, and then
even more after fixing it. In this article we have presented a framework
that provides criteria to define numerically how much testing is
“enough” and “even more.” The same technique also provides a method to
objectively measure the guarantee that the amount of testing performed
provides, when it is not possible to test “enough.”</p>
<p>We have also provided a real example (even though the bug itself is
artificial) where the framework is applied.</p>
<p>As usual, the source code of this page (R scripts, etc) can be found
and downloaded in <a href="https://github.com/lpenz/lpenz.github.io"
class="uri">https://github.com/lpenz/lpenz.github.io</a></p>
