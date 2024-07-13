<div class="body" id="body">
<p>
On some environments, disk space usage can be pretty predictable. In this post,
we will see how to do a linear regression to estimate when free space will reach
zero, and how to assess the quality of such regression, all using
<a href="https://en.wikipedia.org/wiki/R_programming_language">R</a> - the
statistical software environment.
</p>

<section>
<h1>Prerequisites</h1>

<p>
The first thing we need is the data. By running a simple
<code>(date --utc; df -k; echo) &gt;&gt; /var/dflog.txt</code>
everyday at 00:00 by cron, we will have more than enough, as that will store the
date along with total, free and used space for all mounted devices.
</p>
<p>
On the other hand, that is not really easy to parse in R, unless we learn more
about the language. In order to keep this post short, we invite the reader to
use his favorite scripting language (or python) to process that into a file with
the day in the first column and the occupied space in the second, and a row for
each day:
</p>

<pre>
YYYY-MM-DD free space
YYYY-MM-DD free space
(...)
</pre>

<p>
This format can be read and parsed in R with a single command.
</p>
<p>
<a href="$cwd$/duinfo.dat">This</a> is the data file we will use as source for the results
provided in this article. Feel free to download it and repeat the process.
All number in the file are in MB units, and we assume an HD of 500GB. We will
call the date the free space reaches 0 as the <strong>df0</strong>.
</p>

</section>
<section>
<h1>Starting up</h1>

<p>
After running <strong>R</strong> in the shell prompt, we get the usual license and basic help
information.
</p>
<p>
The first step is to import the data:
</p>

<pre>
&gt; duinfo &lt;- read.table('duinfo.dat', colClasses=c("Date","numeric"), col.names=c("day","usd"))
&gt; attach(duinfo)
&gt; totalspace &lt;- 500000
</pre>

<p>
The variable <em>duinfo</em> is now a list with two columns: <em>day</em> and <em>usd</em>. The
<code>attach</code> command allows us to use the column names directly. The
<em>totalspace</em> variable is there just for clarity in the code.
</p>
<p>
We can check the data graphically by issuing:
</p>

<pre>
&gt; plot(usd ~ day, xaxt='n')
&gt; axis.Date(1, day, format='%F')
</pre>

<p>
That gives us an idea on how predictable the usage of our hard drive is.
</p>
<p>
From our example, we get:
</p>
<p>
 <img class="img-responsive" class="center" src="$cwd$/pointplot.png" alt="">
</p>

</section>
<section>
<h1>Linear model</h1>

<p>
We can now create and take a look at our linear model object:
</p>

<pre>
&gt; model &lt;- lm(usd ~ day)
&gt; model
</pre>

<pre>

Call:
lm(formula = usd ~ day)

Coefficients:
(Intercept)          day
 -6424661.2        466.7

</pre>

<p>
The second coefficient in the example tells us that we are consuming about 559 MB of disk space per day.
</p>
<p>
We can also plot the linear model over our data:
</p>

<pre>
&gt; abline(model)
</pre>

<p>
The example plot, with the line:
</p>
<p>
 <img class="img-responsive" class="center" src="$cwd$/lmplot.png" alt="">
</p>

</section>
<section>
<h1>Evaluating the model</h1>

<p>
R provides us with a very generic command that generates statistical information
about objects: <strong>summary</strong>. Let's use it on our linear model objects:
</p>

<pre>
&gt; summary(model)
</pre>

<pre>

Call:
lm(formula = usd ~ day)

Residuals:
    Min      1Q  Median      3Q     Max
-3612.1 -1412.8   300.7  1278.9  3301.0

Coefficients:
              Estimate Std. Error t value Pr(&gt;|t|)
(Intercept) -6.425e+06  3.904e+04  -164.6   &lt;2e-16 ***
day          4.667e+02  2.686e+00   173.7   &lt;2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1697 on 161 degrees of freedom
Multiple R-squared:  0.9947,	Adjusted R-squared:  0.9947
F-statistic: 3.019e+04 on 1 and 161 DF,  p-value: &lt; 2.2e-16

</pre>

<p>
To check the quality of a linear regression, we focus on the <strong>residuals</strong>, as
they represent the error of our model. We calculate them by subtracting the
expected value (from the model) from the sampled value, for every sample.
</p>
<p>
Let's see what each piece of information above means: the first is the
<a href="https://en.wikipedia.org/wiki/Five-number_summary">five-number summary</a>
of the residuals. That tells us the maximum and minimum error, and that 75% of
the errors are between -1.4 GB and 1.3 GB. We then get the results of a
<a href="https://en.wikipedia.org/wiki/Student%27s_t-test">Student's t-test</a> of
the model coefficients against the data. The last column tells us roughly how
probable seeing the given residuals is, assuming that the disk space does not
depend on the date - it's the
<a href="https://en.wikipedia.org/wiki/P-value">p-value</a>. We usually accept an
hypothesis when the p-value is less than 5%; in this example, we have a large
margin for both coefficients. The last three lines of the summary give us more
measures of fit: the
<a href="https://en.wikipedia.org/wiki/R-squared">r-squared</a> values - the closest
to 1, the better; and the general p-value from the f-statistics, less than 5%
again.
</p>
<p>
In order to show how bad a linear model can be, the summary bellow was generated
by using 50GB as the disk space and adding a random value between -1GB and 1GB
each day:
</p>

<pre>

Call:
lm(formula = drand$usd ~ drand$day)

Residuals:
     Min       1Q   Median       3Q      Max
-1012.97  -442.62   -96.19   532.27  1025.01

Coefficients:
             Estimate Std. Error t value Pr(&gt;|t|)
(Intercept) 17977.185  33351.017   0.539    0.591
drand$day       2.228      2.323   0.959    0.340

Residual standard error: 589.7 on 84 degrees of freedom
Multiple R-squared:  0.01083,	Adjusted R-squared:  -0.0009487
F-statistic: 0.9194 on 1 and 84 DF,  p-value: 0.3404

</pre>

<p>
It's easy to notice that, even though the five-number summary is narrower, the
p-values are greater than 5%, and the r-squared values are very far from 1. That
happened because the residuals are not normally distributed.
</p>
<p>
Now that we are (hopefully) convinced that our linear model fits our data
well, we can use it to predict hard-disk shortage.
</p>

</section>
<section>
<h1>Predicting disk-free-zero</h1>

<p>
Until now, we represented disk space as a function of time, creating a model
that allows us to predict the used disk space given the date. But what we really
want now is to predict the date our disk will be full. In order to do that, we
have to invert the model. Fortunately, all statistical properties (t-tests,
f-statistics) hold in the inverted model.
</p>

<pre>
&gt; model2 &lt;- lm(day ~ usd)
</pre>

<p>
We now use the <strong>predict</strong> function to extrapolate the model.
</p>

<pre>
&gt; predict(model2, data.frame(usd = totalspace))
       1
14837.44
</pre>

<p>
But... when is that? Well, that is the numeric representation of a day in R:
the number of days since 1970-01-01. To get the human-readable day, we
use:
</p>

<pre>
&gt; as.Date(predict(model2, data.frame(usd = totalspace)), origin="1970-01-01")
           1
"2010-08-16"
</pre>

<p>
There we are: df0 will be at the above date <strong>if</strong> the
current pattern holds until then.
</p>

</section>
<section>
<h1>Conclusion</h1>

<p>
The linear model can give us the predicted hard disk space usage at any future
date, as long as collected data pattern <strong>is linear</strong>. If the data we collected
has a break point - some disk cleanup or software installation - the model will
not give good results. We will usually see that in the analysis, but we should
also always look at the graph.
</p>
<p>
This article is focused on teaching R basics - data input and plotting. We skip
most of the formalities of science here, and linear regression is certainly not
a proper df0 prediction method in the general case.
</p>
<p>
On the other hand, in the <a href="$cwd$/../df0pred-2/index.html">next part</a> of this
article we will see a more robust method for df0 prediction. We will also
sacrifice our ability to see the used space vs time to get a
statistical distribution for the date of exhaustion, which is a lot more useful
in general.
</p>

</section>
<section>
<h1>Further reading</h1>

<ul>
<li><a href="http://www.cyclismo.org/tutorial/R/index.html">http://www.cyclismo.org/tutorial/R/index.html</a>: R tutorial
</li>
<li><a href="http://www.r-tutor.com/">http://www.r-tutor.com/</a>: An R introduction to statistics
</li>
<li><a href="https://www.datacamp.com/courses/free-introduction-to-r">https://www.datacamp.com/courses/free-introduction-to-r</a>: Datacamp's
  Introduction to R course
</li>
<li><a href="http://cran.r-project.org/doc/contrib/Lemon-kickstart/index.html">http://cran.r-project.org/doc/contrib/Lemon-kickstart/index.html</a>: Kickstarting R
</li>
<li><a href="http://data.princeton.edu/R/linearModels.html">http://data.princeton.edu/R/linearModels.html</a>: "Linear models" page of
  Introduction to R.
</li>
<li><a href="http://www.r-bloggers.com/">http://www.r-bloggers.com/</a>: daily news and tutorials about R, very good to
  learn the language and see what people are doing with it.
</li>
</ul>

</section>
</div>
