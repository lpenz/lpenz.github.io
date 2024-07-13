Haskell eDSL Tutorial - Shared expenses

2011-02-02



People have created
[interesting http://ashish.typepad.com/ashishs_niti/2007/06/another_dsl_emb.html]
and
[weird http://augustss.blogspot.com/2009/02/more-basic-not-that-anybody-should-care.html]
embedded domain-specific languages in haskell. In this article, we will see what
an eDSL really is by building one to record shared expenses and calculate the
payments.

This article is written in literal-haskell style, so that you can simply paste it in a
//file.lhs// and then //runhaskell file.lhs// to run it. That's why we need the
lines bellow:

```

> import Prelude as P
> import Data.Map as Map
> import Control.Monad.State

```



= Why haskell =

The first reason to use haskell for an eDSL is that it has a very clean syntax:
- a function and its arguments are separated by spaces, not by parenthesis and
  commas - parenthesis are only used to make nested function calls;
- there is a syntax sugar for monads that let us avoid writing the monadic
  operators //>>// and //>>=// - it allows us to bind monads by using newlines;
- type inference allows us to skip type annotations and declarations, even
  though we are using a strongly typed language.


When combined, these features allow us to leave very little haskell in our
eDSL, as we will see.



= The shared expenses problem =

Lets say that you went on a trip with 3 friends, and there are some costs that
are shared by everyone. You want to record these expenses and then everyone can
pay up once the trip is over.

In other words, you want records to look like this:

```

> trip = sharedexpenses $ do
>     dexter `spent` 5300
>     angel  `spent` 2700
>     debra  `spent`  800
>     harry  `spent` 1900
>     debra  `spent` 1700
>     angel  `spent` 2200

```

Also, you want to be able to record transactions in which people lend money
straight to each other:

```

>     dexter `gave` harry $ 2000
>     angel  `gave` debra $ 3200

```

The haskell leaks we have in the records are the backticks (`````) and the
``$``. We could get rid of them also, but the plumbing would get a lot more
convoluted. We also avoid using floating point numbers for a similar reason.



= The state monad =

By programming a new monad you get the "programmable semicolon" that people talk
so much about. That allows you to make a custom program flow different from
the standard top-down - it allows built-in backtracking, for instance.

But that is not an eDSL requirement. For our shared-expenses example, top-down
is just fine. The only thing we need is a way to store the expenses each person
had, and a simple
[state monad http://hackage.haskell.org/package/mtl/docs/Control-Monad-State.html]
with a
[map http://hackage.haskell.org/package/containers/docs/Data-Map.html]
inside can solve our problem.

In the next step we define what a person is and who our friends are:
```

> newtype Person = Person { name :: String } deriving (Eq, Ord, Show)

> dexter = Person "Dexter"
> angel  = Person "Angel"
> debra  = Person "Debra"
> harry  = Person "Harry"

```

We could skip this step and just use strings here, but that would make typos a
runtime mistake; by using a strong type and defining the friends explicitly, we
make typos a compile error.



= Spending and giving =

//spent// and //gave// are functions that update our state:

```

> spent :: Person -> Int -> State (Map Person Int) ()
> spent payer money = modify $ insertWith (+) payer money

> gave :: Person -> Person -> Int -> State (Map Person Int) ()
> gave lender borrower money = modify $ (adjust (+ money) lender) . (adjust (\ m -> m - money) borrower)

```

//spent// adds the given amount to the element indexed by the person in the map,
while //gave// adds the amount to the lender and subtract it from the borrower.



= Solving =

To solve the shared expenses problem, we will use a simple algorithm: he who
owes more pays to the one that has more credit until everybody gets paid.

```

> solve st = solve' err $ Map.map ( \ m -> m - avg) st
>     where
>         err = 1 + size st
>         avg = round $ (toRational $ P.foldr (+) 0 st) / (toRational $ size st)

> solve' _   st | Map.null st = []
> solve' err st =
>     (name payer ++ " pays " ++ show amount ++ " to " ++ name receiver) : solve' err newstate
>     where
>         (payer,    debt)   = foldrWithKey (getpers True)  (Person "", 0) st
>         (receiver, credit) = foldrWithKey (getpers False) (Person "", 0) st
>         getpers True  p m (_,  m0) | m < m0 = (p, m) -- Gets payer.
>         getpers False p m (_,  m0) | m > m0 = (p, m) -- Gets receiver.
>         getpers _     _ _ e                 = e
>         amount = min (-debt) credit
>         newstate = Map.filter ( \ c -> c < -err || err < c) $ mapWithKey statefix st
>         statefix p m | p == receiver = m - amount
>         statefix p m | p == payer = m + amount
>         statefix _ m = m

```

The //solve// functions subtracts from everybody the amount that each person is
supposed to spend (the average); the map now has the amount each person is
supposed to pay (negative) or receive (positive). When the amount in the map is
near 0, the person will not be involved in further transactions and is removed
from the map. "Near" here has a precise meaning: we take the number of persons
as the error (plus one), as we had to divide the total amount spent by it in
order to get the average spent - we will not be able to be more precise that
that using integers. Well, we are talking about money, it's useless to be more
precise than a cent anyway.

The //solve'// function recursively registers payments and removes persons from
the map until it is empty. That does not guarantee the least amount of payments,
but we get good enough results most of the times - and it is a lot simpler than
linear programming.



= Plumbing =

The function ``sharedexpenses`` is the one that glues the eDSL and the state
monad, while the ``main`` function is the one that plugs it with
the ``solve`` function and prints the results:

```

> sharedexpenses :: State (Map Person Int) () -> Map Person Int
> sharedexpenses f = execState f empty

> main = mapM_ putStrLn $ solve trip

```

Running this program provides us the following results:

%!include: ``results.txt``



= Conclusions =

We have seen what is an eDSL by building a solution to a real, day-to-day
problem. No hidden enchantments or dark arts involved: you don't have to build a
custom monad or start with something that looks like
//data Expr a = ...//; you can just define how you want your language to look
like, think about what the state needs to store and build the plumbing around
the state monad - or even around the
[writer monad http://hackage.haskell.org/package/mtl/docs/Control-Monad-Writer.html].
You can also use nested state monads to define heterogeneous environments with
different "syntaxes" verified by the type system. The only drawback is that
every user of your language will need a haskell compiler installed, but with
the [haskell platform http://www.haskell.org/platform/] available, that
shouldn't be a problem.



= Further reading =

- http://paulspontifications.blogspot.com.br/2008/01/why-haskell-is-good-for-embedded-domain.html:
  Why Haskell is Good for Embedded Domain Specific Languages
- https://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/:
  Practical Haskell: shell scripting with error handling and privilege separation
- http://gbacon.blogspot.com/2009/07/programmable-semicolon-explained.html:
  Programmable semicolon explained



