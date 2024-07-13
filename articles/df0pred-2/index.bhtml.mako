<div class="body" id="body">
<p>
On the <a href="$cwd$/../df0pred-1/index.html">first</a> article, we saw a quick-and-dirty method to
predict disk space exhaustion when the usage pattern is rigorously linear. We did that by
importing our data into <a href="https://en.wikipedia.org/wiki/R_programming_language">R</a>
and making a linear regression.
</p>
<p>
In this article we will see the problems with that method, and deploy a
more robust solution. Besides robustness, we will also see how we can generate a
probability distribution for the date of disk space exhaustion instead of
calculating a single day.
</p>

<section>
<h1>The problem with the linear regression</h1>

<p>
The linear regression used in the first article has a serious
lack of <a href="https://en.wikipedia.org/wiki/Robust_statistics">robustness</a>.
That means that it is very sensitive to even single departures
from the linear pattern. For instance, if we periodically delete some big
files in the hard disk, we end up breaking the sample in parts that cannot be
analysed together. If we plot the line given by the linear model, we can see
clearly that it does not fit our overall data very well:
</p>
<p>
 <img class="img-responsive" class="center" src="$cwd$/lm.png" alt="">
</p>
<p>
(<a href="$cwd$/duinfospike.dat">Data file</a>)
</p>
<p>
We can see in the graph that the linear model gives us a line that our free disk
space is increasing instead of decreasing! If we use this model, we will reach
the conclusion that we will never reach df0.
</p>
<p>
If we keep analysing used disk space, there is not much we can do besides
discarding the data gathered before the last cleanup. There is no way to easily
ignore only the cleanup.
</p>
<p>
In fact, we can only use the linear regression method when our disk consumption
pattern is linear for the analysed period - and that rarely is the case
when there is human intervention. We should always look at the graph to see if
the model makes sense.
</p>

</section>
<section>
<h1>A na&iuml;ve new method: averaging the difference</h1>

<p>
Instead of using the daily used disk space as input, we will use the
daily <strong>difference</strong> (or delta) of used disk space. By itself, this reduces a
big disk cleanup to a single outlier instead of breaking our sample. We could
then just filter out the outliers, calculate the average daily increment in used
disk space and divide the current free space by it. That would give us the
average number of days left until disk exhaustion. Well, that would also give us
some new problems to solve.
</p>
<p>
The first problem is that filtering out the outliers is neither
straightforward nor recommended. Afterall, we are throwing out data that might
be meaningful: it could be a regular monthly process that we should take into
account to generate a better prediction.
</p>
<p>
Besides, by averaging disk consumption and dividing free disk space by it,  we
would still not have the probability distribution for the date, only a single
value.
</p>

</section>
<section>
<h1>The real new method: days left by Monte Carlo simulation</h1>

<p>
Instead of calculating the number of days left from the data, we will use a
technique called <a href="https://en.wikipedia.org/wiki/Monte_carlo_simulation">Monte Carlo simulation</a>
to generate the distribution of days left. The idea is simple: we sample the
data we have - daily used disk space - until the sum is above the free disk
space; the number of samples taken is the number of days left. By doing that
repeatedly, we get the set of "possible days left" with a distribution that
corresponds to the data we have collected. Let's how we can do that in R.
</p>
<p>
First, let's load the data file that we will use (same one used in the
introduction) along with a variable that holds the size of the disk (500GB; all
units are in MB):
</p>

<pre>

duinfo &lt;- read.table('duinfospike.dat',
		colClasses=c("Date","numeric"),
		col.names=c("day","usd"))
attach(duinfo)
totalspace &lt;- 500000
today &lt;- tail(day, 1)

</pre>

<p>
We now get the delta of the disk usage. Let's take a look at it:
</p>

<pre>
dudelta &lt;- diff(usd)
</pre>

<pre>
plot(dudelta, xaxt='n', xlab='')
</pre>

<p>
 <img class="img-responsive" class="center" src="$cwd$/delta.png" alt="">
</p>
<p>
The summary function gives us the five-number summary, while the boxplot shows
us how the data is distributed graphically:
</p>

<pre>
summary(dudelta)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
-29583.00      5.25    301.00    123.37    713.00   4136.00
</pre>

<pre>
boxplot(dudelta)
</pre>

<p>
 <img class="img-responsive" class="center" src="$cwd$/deltabox.png" alt="">
</p>
<p>
The kernel density plot gives us about the same, but in another visual format:
</p>

<pre>
plot(density(dudelta))
</pre>

<p>
 <img class="img-responsive" class="center" src="$cwd$/deltakd.png" alt="">
</p>
<p>
We can see the cleanups right there, as the lower points.
</p>
<p>
The next step is the creation of the sample of the number of days left until
exhaustion. In order to do that, we create an R function that sums values taken
randomly from our delta sample until our free space zeroes, and returns the
number of samples taken:
</p>

<pre>

f &lt;- function(spaceleft) {
    days &lt;- 0
    while(spaceleft &gt; 0) {
        days &lt;- days + 1
        spaceleft &lt;- spaceleft - sample(dudelta, 1, replace=TRUE)
    }
    days
}

</pre>

<p>
By repeatedly running this function and gathering the results, we generate a set
of number-of-days-until-exhaustion that is robust and corresponds to the data we
have observed. This robustness means that we don't even need to remove outliers,
as they will not disproportionally bias out results:
</p>

<pre>
freespace &lt;- totalspace - tail(usd, 1)
daysleft &lt;- replicate(5000, f(freespace))
</pre>

<pre>
plot(daysleft)
</pre>

<p>
 <img class="img-responsive" class="center" src="$cwd$/daysleft.png" alt="">
</p>
<p>
What we want now is the
<a href="https://en.wikipedia.org/wiki/Empirical_distribution_function">empirical cumulative distribution</a>.
This function gives us the probability that we will reach df0 <strong>before</strong> the
given date.
</p>

<pre>
df0day &lt;- sort(daysleft + today)
df0ecdfunc &lt;- ecdf(df0day)
df0prob &lt;- df0ecdfunc(df0day)
</pre>

<pre>
plot(df0day, df0prob, xaxt='n', type='l')
axis.Date(1, df0day, at=seq(min(df0day), max(df0day), 'year'), format='%F')
</pre>

<p>
 <img class="img-responsive" class="center" src="$cwd$/df0ecdf.png" alt="">
</p>
<p>
With the cumulative probability estimate, we can see when we have to start
worrying about the disk by looking at the first day that the probability of df0
is above 0:
</p>

<pre>
df0day[1]
[1] "2010-06-13"
df0ecdfunc(df0day[1])
[1] 2e-04
</pre>

<p>
Well, we can also be a bit more bold and wait until the chances of reaching df0
rise above 5%:
</p>

<pre>
df0day[which(df0prob &gt; 0.05)[1]]
[1] "2010-08-16"
</pre>

<p>
Mix and match and see what a good convention for your case is.
</p>

</section>
<section>
<h1>Conclusion</h1>

<p>
This and the <a href="$cwd$/../df0pred-1/index.html">previous article</a> showed how to use
statistics in R to predict when free hard-disk space will zero.
</p>
<p>
The first article was main purpose was to serve as an introduction to R. There
are many reasons that make linear regression an unsuitable technique for
df0 prediction - the underlying process of disk consumption is certainly not
linear. But, if the graph shows you that the line fits, there is no reason to
ignore it.
</p>
<p>
Monte Carlo simulation, on the other hand, is a powerful and general technique.
It assumes little about the data (non-parameterized), and it can give you
probability distributions. If you want to forecast something, you can always
start recording data and use Monte Carlo in some way to make predictions
<strong>based on the evidence</strong>. Personally, I think we don't do this nearly as often
as we could. Well, <a href="http://www.joelonsoftware.com/items/2007/10/26.html">Joel is even using it to make schedules</a>.
</p>

</section>
<section>
<h1>Further reading</h1>

<ul>
<li><a href="http://www.joelonsoftware.com/items/2007/10/26.html">http://www.joelonsoftware.com/items/2007/10/26.html</a>: Joel's use of Monte Carlo
  to make schedules.
</li>
<li><a href="https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29">https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29</a>: Wikipedia's page
  on bootstrapping, which is clearer than the one on Monte Carlo simulations.
</li>
<li><a href="http://www.r-bloggers.com/">http://www.r-bloggers.com/</a>: daily news and tutorials about R, very good to
  learn the language and see what people are doing with it.
</li>
</ul>

</section>
</div>
