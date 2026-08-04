<div id="toc" style="margin-bottom: 2em;">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#the-input-data" id="toc-the-input-data"><span
class="toc-section-number">1</span> The input data</a></li>
<li><a href="#visualizing-the-possible-scenarios"
id="toc-visualizing-the-possible-scenarios"><span
class="toc-section-number">2</span> Visualizing the possible
scenarios</a></li>
<li><a href="#visualizing-the-distribution-for-specific-days"
id="toc-visualizing-the-distribution-for-specific-days"><span
class="toc-section-number">3</span> Visualizing the distribution for
specific days</a></li>
<li><a href="#visualizing-the-evolution-of-the-distribution"
id="toc-visualizing-the-evolution-of-the-distribution"><span
class="toc-section-number">4</span> Visualizing the evolution of the
distribution</a>
<ul>
<li><a href="#boxplot" id="toc-boxplot"><span
class="toc-section-number">4.1</span> Boxplot</a></li>
<li><a href="#quantile-lines" id="toc-quantile-lines"><span
class="toc-section-number">4.2</span> Quantile lines</a></li>
<li><a href="#quantile-areas" id="toc-quantile-areas"><span
class="toc-section-number">4.3</span> Quantile areas</a></li>
<li><a href="#quantile-heat-map" id="toc-quantile-heat-map"><span
class="toc-section-number">4.4</span> Quantile heat map</a></li>
</ul></li>
<li><a href="#visualizing-past-present-and-future"
id="toc-visualizing-past-present-and-future"><span
class="toc-section-number">5</span> Visualizing past, present and
future</a></li>
<li><a href="#training-set-validation-set"
id="toc-training-set-validation-set"><span
class="toc-section-number">6</span> Training set, validation
set</a></li>
<li><a href="#conclusions" id="toc-conclusions"><span
class="toc-section-number">7</span> Conclusions</a></li>
</ul>
</div>
<p>On the <a href="$cwd$/../df0pred-2/index.html">second</a> article, we
saw how to use a Monte Carlo simulation generate sample of disk space
delta for future dates and calculate the distribution probability of
zeroing free space in the future.</p>
<p>In this article, we will see how we can plot the evolution of
predicted distribution for the occupied disk space. Instead of answering
que question “how likely is that my disk space will zero before date
X?,” we will answer “how much disk space will I need by date X, and with
what probability?”</p>
<h1 data-number="1" id="the-input-data"><span
class="header-section-number">1</span> The input data</h1>
<p><a href="$cwd$/duinfospike.dat">This file</a> has the dataset we will
use as example. It’s the same we used in the second part. The graph
below shows it:</p>
<p><img src="$cwd$/usd.png" /></p>
<p>We now import this data into R:</p>
<pre><code>duinfo &lt;- read.table(&#39;duinfospike.dat&#39;,
        colClasses=c(&quot;Date&quot;,&quot;numeric&quot;),
        col.names=c(&quot;day&quot;,&quot;usd&quot;))
attach(duinfo)
totalspace &lt;- 450000
today &lt;- tail(day, 1)</code></pre>
<p>We then build our simulations for the next 4 months:</p>
<pre><code># Number of Monte Carlo samples
numsimulations &lt;- 10000

# Number of days to simulate
numdays    &lt;- 240

# Simulate:
simulate &lt;- function(data, ndays) {
    delta &lt;- diff(data)
    dssimtmp0 &lt;- replicate(numsimulations, tail(data, 1))
    dssimtmp  &lt;- dssimtmp0
    f &lt;- function(i) dssimtmp &lt;&lt;- dssimtmp + replicate(numsimulations, sample(delta, 1, replace=TRUE))
    cbind(dssimtmp0, mapply(f, seq(1, ndays)))
}
dssim &lt;- simulate(usd, numdays)

# Future days:
fday &lt;- seq(today, today+numdays, by=&#39;day&#39;)</code></pre>
<h1 data-number="2" id="visualizing-the-possible-scenarios"><span
class="header-section-number">2</span> Visualizing the possible
scenarios</h1>
<p>What king of data have we built in our simulations? Each simulation
is built by sampling from the delta samples and adding to the current
disk space for each day in the simulated period. We can say that each
individual simulation is a possible scenario for the next 4 months. The
graph bellow shows the first 5 simulations:</p>
<pre><code>plot(fday, dssim[1,], ylim=c(min(dssim[1:5,]), max(dssim[1:5,])), ylab=&#39;usd&#39;, xlab=&#39;day&#39;, xaxt=&#39;n&#39;, type=&#39;l&#39;)
axis.Date(1, day, at=seq(min(fday), max(fday), &#39;week&#39;), format=&#39;%F&#39;)
lines(fday, dssim[2,])
lines(fday, dssim[3,])
lines(fday, dssim[4,])
lines(fday, dssim[5,])
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcs3.png" /></p>
<p>From this graph we can clearly see that the range of possible values
for the used disk space grows with time. All simulations start with the
same value - the used disk space for today - and grow apart as we sample
from the delta pool.</p>
<p>We can also plot all simulations in a single graph:</p>
<pre><code>plot(fday, dssim[1,], ylim=c(min(dssim), max(dssim)), ylab=&#39;usd&#39;, xlab=&#39;&#39;, xaxt=&#39;n&#39;, type=&#39;l&#39;)
axis.Date(1, day, at=seq(min(fday), max(fday), &#39;week&#39;), format=&#39;%F&#39;)
f &lt;- function(i) lines(fday, dssim[i,])
mapply(f, seq(2, numdays))
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcs.png" /></p>
<p>This plot gives us an idea of the overall spread of the data, but it
fails to show the density. There are 10000 black lines there, with many
of them overlapping one another.</p>
<h1 data-number="3"
id="visualizing-the-distribution-for-specific-days"><span
class="header-section-number">3</span> Visualizing the distribution for
specific days</h1>
<p>There is another way to look at our data: we have created, for each
day, a sample of the possible used disk spaces. We can take any day of
the simulation and look at the density:</p>
<pre><code>dssimchosen &lt;- list(density(dssim[,5]), density(dssim[,15]), density(dssim[,45]), density(dssim[,120]))
colors &lt;- rainbow(length(dssimchosen))
xs &lt;- c(mapply(function(d) d$x, dssimchosen))
ys &lt;- c(mapply(function(d) d$y, dssimchosen))
plot(dssimchosen[[1]], xlab=&#39;usd&#39;, ylab=&#39;dens&#39;,
    xlim=c(min(xs),max(xs)), ylim=c(min(ys),max(ys)), col=colors[1], main=&#39;&#39;)
lines(dssimchosen[[2]], col=colors[2])
lines(dssimchosen[[3]], col=colors[3])
lines(dssimchosen[[4]], col=colors[4])
abline(v=totalspace, col=&#39;gray&#39;)
legend(&#39;top&#39;, c(&#39;5 day&#39;, &#39;15 days&#39;, &#39;45 days&#39;, &#39;120 days&#39;), fill=colors)</code></pre>
<p><img src="$cwd$/mcsdaydens.png" /></p>
<p>By looking at this graph we can see the trend:</p>
<ul>
<li>The curves are getting flatter: we are getting more possible values
for occupied disk space.</li>
<li>The curves are moving to the right: we have more simulations with
higher occupied disk space values.</li>
</ul>
<h1 data-number="4"
id="visualizing-the-evolution-of-the-distribution"><span
class="header-section-number">4</span> Visualizing the evolution of the
distribution</h1>
<p>So far, we have seen how we can visualize some simulations along the
4 months and how we can visualize the distribution for some specific
days.</p>
<p>We can also plot the distribution of the values for each day in the
simulated 4 months. We can’t use the kernel density plot or the
histogram, as they use both axes, but there are other options, most of
them involving some abuse of the built-in plot functions.</p>
<h2 data-number="4.1" id="boxplot"><span
class="header-section-number">4.1</span> Boxplot</h2>
<p>We can use the <em>boxplot</em> function to create a boxplot for each
day in R in a very straightforward way:</p>
<pre><code>boxplot(dssim, outline=F, names=seq(today, as.Date(today+numdays), by=&#39;day&#39;), ylab=&#39;usd&#39;, xlab=&#39;day&#39;, xaxt=&#39;n&#39;)
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcsbox.png" /></p>
<p>The boxplots glued together form a shape that shows us the
distribution of our simulations at any day:</p>
<ul>
<li>The thick line in the middle of the graph is the median</li>
<li>The darker area goes from the first quartile to the third - which
means that 50% of the samples are in that range</li>
<li>The lighter area has the maximum and minimum points, if they are
within 1.5 <a
href="https://en.wikipedia.org/wiki/Interquartile_range">IQR</a> of the
upper/lower quartile. Points out of this range are considered outliers
and are not plotted.</li>
</ul>
<h2 data-number="4.2" id="quantile-lines"><span
class="header-section-number">4.2</span> Quantile lines</h2>
<p>We can use the <em>quantile</em> function to calculate the values of
each <a href="https://en.wikipedia.org/wiki/Quantile">quantile</a> per
day, and plot the lines:</p>
<pre><code>q &lt;- 6
f &lt;- function(i) quantile(dssim[,i], seq(0, 1, 1.0/q))
qvals &lt;- mapply(f, seq(1, numdays+1))
colors &lt;- colorsDouble(rainbow, q+1)
plot(fday, qvals[1,], ylab=&#39;usd&#39;, xlab=&#39;day&#39;, xaxt=&#39;n&#39;, type=&#39;l&#39;, col=colors[1], ylim=c(min(qvals), max(qvals)))
mapply(function(i) lines(fday, qvals[i,], col=colors[i]), seq(2, q+1))
axis.Date(1, day, at=seq(min(fday), max(fday), &#39;week&#39;), format=&#39;%F&#39;)
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcsquant.png" /></p>
<p>The advantage of this type of graph over the boxplot is that it is
parameterized by <em>q</em>. This variable tells us the number of parts
that we should divide our sample in. The lines above show us the
division. If <em>q</em> is odd, the middle line is exactly the median.
If <em>q</em> is 4, the lines will draw a shape similar to that of the
boxplot, the only difference being the top and bottom line, that will
include outliers - the boxplot filters outliers by using the IQR as
explained above.</p>
<p>In the code above, we have used the <em>colorsDouble</em> function to
generate a sequence of colors that folds in the middle:</p>
<pre><code>colorsDouble &lt;- function(colorfunc, numcolors) {
    colors0 &lt;- rev(colorfunc((1+numcolors)/2))
    c(colors0, rev(if (numcolors %% 2 == 0) colors0 else head(colors0, -1)))
}</code></pre>
<h2 data-number="4.3" id="quantile-areas"><span
class="header-section-number">4.3</span> Quantile areas</h2>
<p>We can also abuse the <em>barplots</em> function to create an area
graph. We have to eliminate the bar borders, zero the distance between
them and plot a white bar from the axis to the first quartile, if
appropriate:</p>
<pre><code>q &lt;- 7
f &lt;- function(i) {
    qa &lt;- quantile(dssim[,i], seq(0, 1, 1.0/q))
    c(qa[1], diff(qa))
}
qvals &lt;- mapply(f, seq(1, numdays+1))
colors &lt;- c(&#39;white&#39;, colorsDouble(rainbow, q))
barplot(qvals, ylab=&#39;usd&#39;, xlab=&#39;day&#39;, col=colors, border=NA, space=0,
    names.arg=seq(min(fday), max(fday), &#39;day&#39;), ylim=c(min(dssim), max(dssim)))
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcsquantbar.png" /></p>
<p>In this case, using an odd <em>q</em> makes more sense, as we want to
use the same colors for the symmetric intervals. With an even
<em>q</em>, there would either be a larger middle interval with two
quantiles or a broken symmetry. The code above builds a larger middle
interval when given an even <em>q</em>.</p>
<h2 data-number="4.4" id="quantile-heat-map"><span
class="header-section-number">4.4</span> Quantile heat map</h2>
<p>If we increase <em>q</em> and use <em>heat.colors</em> in a quantile
area plot, we get something similar to a heat map:</p>
<pre><code>q &lt;- 25
f &lt;- function(i) {
    qa &lt;- quantile(dssim[,i], seq(0, 1, 1.0/q))
    c(qa[1], mapply(function(j) qa[j] - qa[j-1], seq(2, q+1)))
}
qvals &lt;- mapply(f, seq(1, numdays+1))
colors &lt;- c(&#39;white&#39;, colorsDouble(heat.colors, q))
barplot(qvals, ylab=&#39;usd&#39;, xlab=&#39;day&#39;, col=colors, border=NA, space=0,
    names.arg=seq(min(fday), max(fday), &#39;day&#39;), ylim=c(min(dssim), max(dssim)))
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcsquantheat.png" /></p>
<h1 data-number="5" id="visualizing-past-present-and-future"><span
class="header-section-number">5</span> Visualizing past, present and
future</h1>
<p>We can also plot our data in the same graph as our simulations, by
extending the axis of the <em>barplot</em> and using the <em>points</em>
function:</p>
<pre><code>quantheatplot &lt;- function(x, sim, ylim) {
    q &lt;- 25
    simstart &lt;- length(x) - length(sim[1,])
    f &lt;- function(i) {
        if (i &lt; simstart)
            replicate(q+1, 0)
        else {
            qa &lt;- quantile(sim[,i-simstart], seq(0, 1, 1.0/q))
            c(qa[1], diff(qa))
        }
    }
    qvals &lt;- mapply(f, seq(1, length(x)))
    colors &lt;- c(&#39;white&#39;, colorsDouble(heat.colors, q))
    barplot(qvals, ylab=&#39;usd&#39;, xlab=&#39;day&#39;, col=colors, border=NA, space=0,
        names.arg=x, ylim=ylim)
    abline(h=totalspace, col=&#39;gray&#39;)
}</code></pre>
<pre><code>quantheatplot(c(day, seq(min(fday), max(fday), &#39;day&#39;)), dssim, ylim=c(min(c(usd, dssim)), max(dssim)))
points(usd)
abline(h=totalspace, col=&#39;gray&#39;)</code></pre>
<p><img src="$cwd$/mcspf1.png" /></p>
<h1 data-number="6" id="training-set-validation-set"><span
class="header-section-number">6</span> Training set, validation set</h1>
<p><a
href="https://en.wikipedia.org/wiki/Cross-validation">Cross-validation</a>
is a technique that we can use to validate the use of Monte Carlo on our
data.</p>
<p>We first split our data in two sets: the training set and the
validation set. We than use only the first in our simulations, and plot
the second over. We can then see graphically if the data fits our
simulation.</p>
<p>Let’s use the first two months as the training set, and the other
three months as the validation set:</p>
<pre><code># Number of days to use in the training set
numdaysTrain &lt;- 60
numdaysVal   &lt;- length(day) - numdaysTrain

dssim2 &lt;- simulate(usd[seq(1, numdaysTrain)], numdaysVal-1)</code></pre>
<pre><code>allvals &lt;- c(usd, dssim2)
quantheatplot(day, dssim2, c(min(allvals), max(allvals)))
points(usd)</code></pre>
<p><img src="$cwd$/mcscv1.png" /></p>
<p>Looks like using only the first two months already gives us a fair
simulation. What if we used only a single month, when no disk cleanup
was performed?</p>
<pre><code># Number of days to use in the training set
numdaysTrain &lt;- 30
numdaysVal   &lt;- length(day) - numdaysTrain

dssim3 &lt;- simulate(usd[seq(1, numdaysTrain)], numdaysVal-1)</code></pre>
<pre><code>allvals &lt;- c(usd, dssim3)
quantheatplot(day, dssim3, c(min(allvals), max(allvals)))
points(usd)</code></pre>
<p><img src="$cwd$/mcscv2.png" /></p>
<p>If we do regular disk cleanups, we must have at least one of them in
our training set to get realistic results. Our training set is not
representative without it.</p>
<p>This also tests our cross-validation code. A common mistake is using
the whole data set as the training set and as the validation set. That
is not cross-validation.</p>
<h1 data-number="7" id="conclusions"><span
class="header-section-number">7</span> Conclusions</h1>
<p>We can use Monte Carlo simulations not only to generate a
distribution probability of an event as we did in the <a
href="$cwd$/../df0pred-2/index.html">previous</a> article, but also to
predict a possible range of future values. In this article, disk space
occupation is not the most interesting example, as we are usually more
interested in knowing when our used disk space will reach a certain
value than in knowing the most probable values in time. But imagine that
the data represents the number of miles traveled in a road trip or race.
You can then not only see when you will arrive at your destination, but
also the region where you will probably be at any day.</p>
<p>There are plenty of other uses for this kind of prediction. Collect
the data, look at it and think if it would be useful to predict future
ranges, and if it makes sense with the data you have. Predictions based
on the evidence can be even used to support a decision or a point of
view, just keep mind that you can only use the past if you honestly
don’t think anything different is going to happen.</p>
