---
title: Map Comprehensions
published: 2014-06-05T08:30:00Z
tags: haskell, maps, indexed monads, query, sql, relational algebra, comprehensions, hlist
description: An idea for a useful indexed monad: the map comprehension.
---

Introduction
------------

It is well known that the list comprehension notation from functional
programming bares more than a passing resemblance to SQL queries:

~~~{.SQL}
SELECT c.name, o.price
FROM customer c, orders o
WHERE c.pk_cust_id = o.fk_cust_id
~~~

~~~{.haskell}
[ (name c, price o)
| c <- customer
, o <- orders
, pk_cust_id c == fk_cust_id o ]
~~~

List comprehensions offer a declarative query syntax that is very
SQL-like and readable, but also much more compositional than SQL, for
example we can nest comprehensions arbitrarily deep, create
re-usable abstractions and use recursion.  However, the choice of sets
or lists to collect the rows does not model the mapping of key
attributes to non-key attributes, which is essential for an efficient
database implementation. In this post, we'll explore using a map
representation `Map k v` to model the primary key. A map does force us
to work with all data in memory, unlike streams, but then complex
queries with grouping and ordering usually force this on us anyway.
Map semantics also show up often in distributed systems, for example
NoSQL stores and distributed caches.

Haskell's Data.Map already includes implementations for many of the
standard relational algebra operations, such as union, intersection
and difference. But if we want to explore what a 'map comprehension'
might look like, we need to take a look at *indexed monads*.

Indexed Monads
--------------

Indexed monads are a generalisation of monads where a monadic type
constructor is indexed by another type representing additional
information that further categorises the computation or structure.

~~~{.haskell}
join   : m s (m t a) -> m (s ⨂ t) a
return : m ε a
~~~

The monad indices are a monoid (K, ⨂, ε). Dominic Orchard has
published some great slides on indexed monads [here][IxMonad].

In order to see what a map comprehension might look like, we are going
to need a structure that is both a value-level and a type-level monoid
to model the key attributes. Luckily, GHC's new DataKinds extension
makes it very easy to implement a *heterogeneous list*
(HList). Ideally, we probably want set semantics for our
key-attributes, but an HList satisfies the minimum monoid requirement.

Firstly, since this is a literal Haskell post, we'll need to enable
some extensions and import some modules:

> {-# LANGUAGE GADTs               #-}
> {-# LANGUAGE TypeOperators       #-}
> {-# LANGUAGE KindSignatures      #-}
> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE TypeFamilies        #-}
> {-# LANGUAGE FlexibleInstances   #-}
> {-# LANGUAGE FlexibleContexts    #-}
> {-# LANGUAGE ConstraintKinds     #-}
> {-# LANGUAGE MonadComprehensions #-}
> {-# LANGUAGE TransformListComp   #-}
> {-# LANGUAGE RebindableSyntax    #-}

> import Prelude hiding (Monad(..), fmap, sum, head, tail)
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Monoid


Heterogeneous Lists using Data Kinds
------------------------------------

Let's roll our own simple HList implementation using the [DataKinds][DataKinds]
extension. We will need a type-level append and a value-level append.

> -- | The heterogeneous list
> data HList :: [*] -> * where
>   Z    :: HList '[]
>   (:.) :: t -> HList ts -> HList (t ': ts)
>
> infixr 5 :.
>
> -- | HList type-level append
> type family (m :: [*]) ++ (n :: [*]) :: [*]
> type instance '[]       ++ ys = ys
> type instance (x ': xs) ++ ys = x ': (xs ++ ys)
>
> -- | HList value-level append
> (++.) :: HList as -> HList bs -> HList (as ++ bs)
> Z         ++. ys = ys
> (x :. xs) ++. ys = x :. (xs ++. ys)
>
> infixr 5 ++.
>
> head :: HList (a ': as) -> a
> head (x :. _) = x
>
> tail :: HList (a ': as) -> HList as
> tail (_ :. xs) = xs

Now we can compute the union of two key-attributes. For example:

~~~
λ> (42 :. Z) ++. ("foo" :. Z)
42:."foo":.Z
~~~

The Map Comprehension
---------------------

To write a map comprehension, we will use the the RebindableSyntax and
MonadComprehensions extensions. In theory we could bind this syntax to
the methods in an indexed monad class. It should also be possible to
make Map instances of an indexed functor and indexed monad class
hierarchy, but that is beyond the scope of this post.

First, some useful type and constraint synonyms:

> type K = HList
> type Key k = Ord (K k)
> type Union k k' = (Key k, Key k', Key (k ++ k'))

A Map is an indexed functor, whereby the fmap operation does not
change the index type. This is consistent with Data.Map.map which
leaves the keys alone:

> -- | projection
> fmap :: Key k => (v -> v') -> Map (K k) v -> Map (K k) v'
> fmap = Map.map

The monadic join operation for Map is analogous to list
concatenation. Not surprisingly, there is nothing in Data.Map for
this, as we need to union the map keys. We can implement this if we
make the assumption that the keys will have a monoid append
operation. Here we use HList append (++.):

> -- | monadic join or "concat"
> join :: Union k k' => Map (K k) (Map (K k') v) -> Map (K (k ++ k')) v
> join = Map.foldrWithKey (\k m r ->
>          Map.foldrWithKey (\k' v r' ->
>            Map.insert (k ++. k') v r') r m) Map.empty

For return, we need to introduce the concept of a null-key, which we
represent as an empty HList:

> return :: v -> Map (K '[]) v
> return x = Map.singleton Z x

Monadic bind can simply be defined in terms of join and fmap:

> (>>=) :: Union k k' => Map (K k) v -> (v ->  Map (K k') v') -> Map (K (k ++ k')) v'
> m >>= f = join (fmap f m)

> (>>) :: Union k k' => Map (K k) v -> Map (K k') v' -> Map (K (k ++ k')) v'
> x >> y = x >>= (\_ -> y)

> fail = error "failed"

Let's try a simple cartesian product (also known as a relational
cross-join). We'll also use an HList for our non-key attributes,
although the map comprehension does not require this:

> example1 = [ x ++. y
>            | x <- Map.fromList [ ("foo":.Z, 1:.Z), ("bar":.Z, 2:.Z) ]
>            , y <- Map.fromList [ ("baz":.Z, 3:.Z), ("qux":.Z, 4:.Z) ]
>            ]

~~~
λ> example1
fromList [("bar":."baz":.Z, 2:.3:.Z)
         ,("bar":."qux":.Z, 2:.4:.Z)
         ,("foo":."baz":.Z, 1:.3:.Z)
         ,("foo":."qux":.Z, 1:.4:.Z)]
~~~

Note that the result contains new keys which are the cartesian product
of the original keys. The values are exactly what we asked for, which
in this instance, is also a cartesian product. I've also taken the liberty
of formatting the output, for ease of reading.

From the above example, we can see that a map comprehension takes care
of deriving key attributes for us. It is more restrictive than combining a
list comprehension with Map.fromList, as we cannot create arbitrary
keys.

To filter out values, let's rebind the monadic guard function:

> guard :: Bool -> Map (K '[]) ()
> guard True  = return ()
> guard False = Map.empty

Now we can use guards in the comprehension syntax:

>
> -- | guards filter on values, they cannot access keys
> example2 = [ x + y
>            | x <- Map.fromList [ ("foo":.Z, 1), ("bar":.Z, 2) ]
>            , y <- Map.fromList [ ("baz":.Z, 3), ("qux":.Z, 4) ]
>            , x + y < 6
>            ]
>

~~~
λ> example2
fromList [("bar":."baz":.Z, 5)
         ,("foo":."baz":.Z, 4)
         ,("foo":."qux":.Z, 5)]
~~~

Again this is more restrictive than a list comprehension, but the
restriction is useful, it forces the user out of the comprehension
guard syntax, if they want to filter by key. In other words, you cannot
use the comprehension guard syntax to represent an inefficient
relational inner-join.

An efficient inner-join would need to be accomplished using an
auxiliary function such as the one below. Again, we use an HList for
value attributes, so that we can append them together.

> innerJoin
>   :: (Key k, Key k') =>
>      (HList as -> K k')
>    -> Map (K k)  (HList as)
>    -> Map (K k') (HList bs)
>    -> Map (K k)  (HList (as ++ bs))
> innerJoin f m1 m2 = Map.foldrWithKey doRow Map.empty m1
>   where
>     doRow k v m = maybe m (\v' -> Map.insert k (v ++. v') m) $ Map.lookup (f v) m2
>

Aggregation and Grouping
------------------------

In SQL, aggregation is the reduction of a row set by an associative
and commutative binary operation. It is the most significant extension
of the relational algebra in terms of expressiveness. Since all SQL
queries must return sets of primitive atomic values, aggregation
functions need to be coupled with grouping clauses. If we don't have
this restriction, we are free to completely separate aggregation from
grouping.

Note that 'groupWith' when applied to a key constructor, yields a grouping
function that conceptually is dual to concatenation (monadic join), though
that's not quite the case here as we do not split up the input key.

> -- | grouping for potential aggregation
> groupWith :: (Key k, Key k') =>
>              (v -> K k') -> Map (K k) v -> Map (K k') (Map (K k) v)
> groupWith f = Map.foldrWithKey group Map.empty
>    where
>      group k v = Map.insertWith Map.union (f v) (Map.singleton k v)

> -- | aggregate or reduce
> aggregate :: Monoid m => Map (K k) m -> m
> aggregate = Map.foldr mappend mempty

> sum :: (Num a, Key k) => Map (K k) a -> a
> sum = getSum . aggregate . fmap Sum

> count :: (Num a, Key k) => Map (K k) v -> a
> count = sum . fmap (const 1)

The [TransformListComp][TransformListComp] extension allows us to use
SQL-like "group by" clauses in our monad comprehensions. In the
example below, we group and count the numeric values from an input
map. A rather idiosyncratic feature of the TransformListComp
extension is that the variable `x` gets rebound to be the grouping
result, in this case a map from the grouping key to the group. Here
the rebinding is convenient and we can simply call 'count' on it to
perform an aggregation.


> example3 = [ count x
>            | x <- Map.fromList [ ("foo":.Z, 1), ("bar":.Z, 2)
>                                , ("baz":.Z, 2), ("qux":.Z, 3)]
>            , then group by (x:.Z) using groupWith
>            ]
>

~~~
λ> example3
fromList [(1:.Z, 1])
         ,(2:.Z, 2])
         ,(3:.Z, 1])]
~~~

It is quite straightforward to create a flat pivot table using a
compound grouping key:

> example4 = [ sum (fmap orderAmt x)
>            | x <- orders
>            , then group by (orderIsin x :. orderTrader x :.Z) using groupWith
>            ]

> orders = Map.fromList
>   [ (1:.Z, "US68389X1054":."John":.10.0:.Z)
>   , (2:.Z, "US0378331005":."Joe" :. 2.0:.Z)
>   , (3:.Z, "US68389X1054":."John":. 2.0:.Z)
>   , (4:.Z, "US0378331005":."Joe" :. 5.0:.Z)
>   , (5:.Z, "US5949181045":."John":.10.0:.Z) ]
>
> orderIsin   = head        -- ^ we need some Template Haskell here
> orderTrader = head . tail
> orderAmt    = head . tail . tail

~~~
λ> example4
fromList [("US0378331005":."Joe" :.Z, 7.0)
         ,("US5949181045":."John":.Z,10.0)
         ,("US68389X1054":."John":.Z,12.0)]
~~~

However, if we want to produce an output with successive nested maps,
the TransfromListComp syntax is not much help.  It is easier to use
simple function composition of a flat grouping function 'GroupBy':

> -- | an alternative grouping function which performs aggregation
> groupBy :: (Key k, Key k') =>
>            (v -> K k') -> (Map (K k) v -> v') -> Map (K k) v -> Map (K k') v'
> groupBy f g = fmap g . groupWith f

We can compose the groupBy function according to how many dimensions
we want to pivot on. This works because the aggregation function
passed to 'GroupBy' can itself be another grouping operation.  Such a
transformation would be very useful if we were, for example,
converting a flat CSV file into a hierarchical XML file.

> -- | from a flat representation to a hierarchy
> example5  = ( groupBy ((:.Z) . orderIsin)
>             . groupBy ((:.Z) . orderTrader)
>             ) (fmap orderAmt) orders

~~~
λ> example5
fromList [("US0378331005":.Z
             ,fromList [("Joe":.Z
                 ,fromList [(2:.Z, 2.0)
                           ,(4:.Z, 5.0)])])
         ,("US5949181045":.Z
             ,fromList [("John":.Z
                 ,fromList [(5:.Z,10.0)])])
         ,("US68389X1054":.Z
             ,fromList [("John":.Z
                 ,fromList [(1:.Z,10.0)
                           ,(3:.Z, 2.0)])])]
~~~

The reverse of this operation is a series of corresponding cartesian
products using the map comprehension. This is the type of transformation we
would need to go from a hierarchical data representation to a flat one,
for example from XML to CSV.

> -- | from a hierarchy to a flat representation
> example6 = [ amt
>            | isin   <- example5
>            , trader <- isin
>            , amt    <- trader
>            ]

~~~
λ> example5
fromList [("US0378331005":."Joe" :.2:.Z, 2.0)
         ,("US0378331005":."Joe" :.4:.Z, 5.0)
         ,("US5949181045":."John":.5:.Z,10.0)
         ,("US68389X1054":."John":.1:.Z,10.0)
         ,("US68389X1054":."John":.3:.Z,2.0)]
~~~

That's about all I've explored of the 'map comprehension' idea to date.

\

Note: the literal haskell for this entire post can be found [here](https://raw.githubusercontent.com/willtim/timphilipwilliams.com/master/posts/2014-06-05-map-comprehensions.lhs).

* * * * * * * *

References
----------

\[1\] [Fun with Indexed Monads][IxMonad]

\[2\] [Giving Haskell a Promotion][DataKinds]

\[3\] [Comprehensive Comprehensions][TransformListComp]


[IxMonad]: http://www.cl.cam.ac.uk/~dao29/ixmonad/ixmonad-fita14.pdf (Fun with Indexed Monads)
[DataKinds]: http://dreixel.net/research/pdf/ghp.pdf (Giving Haskell a Promotion)
[TransformListComp]: http://research.microsoft.com/en-us/um/people/simonpj/papers/list-comp/list-comp.pdf (Comprehensive Comprehensions)


Appendix
--------

<h4>HList instances</h4>

> instance Show (HList '[]) where
>   show _ = "Z"
> instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
>   show (x:.xs) = show x ++ ":." ++ show xs

> instance Eq (HList '[]) where
>   _ == _ = True
> instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
>   (x:.xs) == (y:.ys) = x == y && xs == ys

> instance Ord (HList '[]) where
>   _ `compare` _ = EQ
> instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs)) where
>   (x:.xs) `compare` (y:.ys) = (x, xs) `compare` (y, ys)
