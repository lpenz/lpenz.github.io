<div id="toc" style="margin-bottom: 2em;">
  <p class="toctitle">Contents</p>
<ul>
<li><a href="#why-haskell" id="toc-why-haskell"><span
class="toc-section-number">1</span> Why haskell</a></li>
<li><a href="#the-shared-expenses-problem"
id="toc-the-shared-expenses-problem"><span
class="toc-section-number">2</span> The shared expenses problem</a></li>
<li><a href="#the-state-monad" id="toc-the-state-monad"><span
class="toc-section-number">3</span> The state monad</a></li>
<li><a href="#spending-and-giving" id="toc-spending-and-giving"><span
class="toc-section-number">4</span> Spending and giving</a></li>
<li><a href="#solving" id="toc-solving"><span
class="toc-section-number">5</span> Solving</a></li>
<li><a href="#plumbing" id="toc-plumbing"><span
class="toc-section-number">6</span> Plumbing</a></li>
<li><a href="#conclusions" id="toc-conclusions"><span
class="toc-section-number">7</span> Conclusions</a></li>
<li><a href="#further-reading" id="toc-further-reading"><span
class="toc-section-number">8</span> Further reading</a></li>
</ul>
</div>
<p>People have created <a
href="http://ashish.typepad.com/ashishs_niti/2007/06/another_dsl_emb.html">interesting</a>
and <a
href="http://augustss.blogspot.com/2009/02/more-basic-not-that-anybody-should-care.html">weird</a>
embedded domain-specific languages in haskell. In this article, we will
see what an eDSL really is by building one to record shared expenses and
calculate the payments.</p>
<p>This article is written in literal-haskell style, so that you can
simply paste it in a <em>file.lhs</em> and then <em>runhaskell
file.lhs</em> to run it. That’s why we need the lines bellow:</p>
<pre><code>&gt; import Data.Map as Map
&gt; import Control.Monad.State</code></pre>
<h1 data-number="1" id="why-haskell"><span
class="header-section-number">1</span> Why haskell</h1>
<p>The first reason to use haskell for an eDSL is that it has a very
clean syntax:</p>
<ul>
<li>a function and its arguments are separated by spaces, not by
parenthesis and commas - parenthesis are only used to make nested
function calls;</li>
<li>there is a syntax sugar for monads that let us avoid writing the
monadic operators <em>&gt;&gt;</em> and <em>&gt;&gt;=</em> - it allows
us to bind monads by using newlines;</li>
<li>type inference allows us to skip type annotations and declarations,
even though we are using a strongly typed language.</li>
</ul>
<p>When combined, these features allow us to leave very little haskell
in our eDSL, as we will see.</p>
<h1 data-number="2" id="the-shared-expenses-problem"><span
class="header-section-number">2</span> The shared expenses problem</h1>
<p>Lets say that you went on a trip with 3 friends, and there are some
costs that are shared by everyone. You want to record these expenses and
then everyone can pay up once the trip is over.</p>
<p>In other words, you want records to look like this:</p>
<pre><code>&gt; trip = sharedexpenses $ do
&gt;     dexter `spent` 5300
&gt;     angel  `spent` 2700
&gt;     debra  `spent`  800
&gt;     harry  `spent` 1900
&gt;     debra  `spent` 1700
&gt;     angel  `spent` 2200</code></pre>
<p>Also, you want to be able to record transactions in which people lend
money straight to each other:</p>
<pre><code>&gt;     dexter `gave` harry $ 2000
&gt;     angel  `gave` debra $ 3200</code></pre>
<p>The haskell leaks we have in the records are the backticks
(<code>`</code>) and the <code>$</code>. We could get rid of them also,
but the plumbing would get a lot more convoluted. We also avoid using
floating point numbers for a similar reason.</p>
<h1 data-number="3" id="the-state-monad"><span
class="header-section-number">3</span> The state monad</h1>
<p>By programming a new monad you get the “programmable semicolon” that
people talk so much about. That allows you to make a custom program flow
different from the standard top-down - it allows built-in backtracking,
for instance.</p>
<p>But that is not an eDSL requirement. For our shared-expenses example,
top-down is just fine. The only thing we need is a way to store the
expenses each person had, and a simple <a
href="http://hackage.haskell.org/package/mtl/docs/Control-Monad-State.html">state
monad</a> with a <a
href="http://hackage.haskell.org/package/containers/docs/Data-Map.html">map</a>
inside can solve our problem.</p>
<p>In the next step we define what a person is and who our friends
are:</p>
<pre><code>&gt; newtype Person = Person { name :: String } deriving (Eq, Ord, Show)

&gt; dexter = Person &quot;Dexter&quot;
&gt; angel  = Person &quot;Angel&quot;
&gt; debra  = Person &quot;Debra&quot;
&gt; harry  = Person &quot;Harry&quot;</code></pre>
<p>We could skip this step and just use strings here, but that would
make typos a runtime mistake; by using a strong type and defining the
friends explicitly, we make typos a compile error.</p>
<h1 data-number="4" id="spending-and-giving"><span
class="header-section-number">4</span> Spending and giving</h1>
<p><em>spent</em> and <em>gave</em> are functions that update our
state:</p>
<pre><code>&gt; spent :: Person -&gt; Int -&gt; State (Map Person Int) ()
&gt; spent payer money = modify $ insertWith (+) payer money

&gt; gave :: Person -&gt; Person -&gt; Int -&gt; State (Map Person Int) ()
&gt; gave lender borrower money = modify $ (adjust (+ money) lender) . (adjust (\ m -&gt; m - money) borrower)</code></pre>
<p><em>spent</em> adds the given amount to the element indexed by the
person in the map, while <em>gave</em> adds the amount to the lender and
subtract it from the borrower.</p>
<h1 data-number="5" id="solving"><span
class="header-section-number">5</span> Solving</h1>
<p>To solve the shared expenses problem, we will use a simple algorithm:
he who owes more pays to the one that has more credit until everybody
gets paid.</p>
<pre><code>&gt; solve st = solve&#39; err $ Map.map ( \ m -&gt; m - avg) st
&gt;     where
&gt;         err = 1 + size st
&gt;         avg = round $ (toRational $ fold (+) 0 st) / (toRational $ size st)

&gt; solve&#39; _   st | Map.null st = []
&gt; solve&#39; err st =
&gt;     (name payer ++ &quot; pays &quot; ++ show amount ++ &quot; to &quot; ++ name receiver) : solve&#39; err newstate
&gt;     where
&gt;         (payer,    debt)   = foldrWithKey (getpers True)  (Person &quot;&quot;, 0) st
&gt;         (receiver, credit) = foldrWithKey (getpers False) (Person &quot;&quot;, 0) st
&gt;         getpers True  p m (_,  m0) | m &lt; m0 = (p, m) -- Gets payer.
&gt;         getpers False p m (_,  m0) | m &gt; m0 = (p, m) -- Gets receiver.
&gt;         getpers _     _ _ e                 = e
&gt;         amount = min (-debt) credit
&gt;         newstate = Map.filter ( \ c -&gt; c &lt; -err || err &lt; c) $ mapWithKey statefix st
&gt;         statefix p m | p == receiver = m - amount
&gt;         statefix p m | p == payer = m + amount
&gt;         statefix _ m = m</code></pre>
<p>The <em>solve</em> functions subtracts from everybody the amount that
each person is supposed to spend (the average); the map now has the
amount each person is supposed to pay (negative) or receive (positive).
When the amount in the map is near 0, the person will not be involved in
further transactions and is removed from the map. “Near” here has a
precise meaning: we take the number of persons as the error (plus one),
as we had to divide the total amount spent by it in order to get the
average spent - we will not be able to be more precise that that using
integers. Well, we are talking about money, it’s useless to be more
precise than a cent anyway.</p>
<p>The <em>solve’</em> function recursively registers payments and
removes persons from the map until it is empty. That does not guarantee
the least amount of payments, but we get good enough results most of the
times - and it is a lot simpler than linear programming.</p>
<h1 data-number="6" id="plumbing"><span
class="header-section-number">6</span> Plumbing</h1>
<p>The function <code>sharedexpenses</code> is the one that glues the
eDSL and the state monad, while the <code>main</code> function is the
one that plugs it with the <code>solve</code> function and prints the
results:</p>
<pre><code>&gt; sharedexpenses :: State (Map Person Int) () -&gt; Map Person Int
&gt; sharedexpenses f = execState f empty

&gt; main = mapM_ putStrLn $ solve trip</code></pre>
<p>Running this program provides us the following results:</p>
<pre><code>Debra pays 4350 to Angel
Harry pays 3650 to Dexter
Harry pays 100 to Angel
</code></pre>
<h1 data-number="7" id="conclusions"><span
class="header-section-number">7</span> Conclusions</h1>
<p>We have seen what is an eDSL by building a solution to a real,
day-to-day problem. No hidden enchantments or dark arts involved: you
don’t have to build a custom monad or start with something that looks
like <em>data Expr a = …</em>; you can just define how you want your
language to look like, think about what the state needs to store and
build the plumbing around the state monad - or even around the <a
href="http://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer.html">writer
monad</a>. You can also use nested state monads to define heterogeneous
environments with different “syntaxes” verified by the type system. The
only drawback is that every user of your language will need a haskell
compiler installed, but with the <a
href="http://www.haskell.org/platform/">haskell platform</a> available,
that shouldn’t be a problem.</p>
<h1 data-number="8" id="further-reading"><span
class="header-section-number">8</span> Further reading</h1>
<ul>
<li><a
href="http://paulspontifications.blogspot.com.br/2008/01/why-haskell-is-good-for-embedded-domain.html"
class="uri">http://paulspontifications.blogspot.com.br/2008/01/why-haskell-is-good-for-embedded-domain.html</a>:
Why Haskell is Good for Embedded Domain Specific Languages</li>
<li><a
href="https://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/"
class="uri">https://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/</a>:
Practical Haskell: shell scripting with error handling and privilege
separation</li>
<li><a
href="http://gbacon.blogspot.com/2009/07/programmable-semicolon-explained.html"
class="uri">http://gbacon.blogspot.com/2009/07/programmable-semicolon-explained.html</a>:
Programmable semicolon explained</li>
</ul>
