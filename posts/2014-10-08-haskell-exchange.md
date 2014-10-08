---
title: Structural Typing for Structured Products
published: 2014-10-08T19:10:00Z
tags: types, type systems, row polymorphism, structural typing, DSLs, compilers
description: Slides and some example code for the Haskell Exchange talk.
---

Today at the **Haskell Exchange 2014**, Peter Marks and I presented
“Structural Typing for Structural Products” (slides available
[here][#slides]).  In the talk, we walk through the motivation that
led us towards using row-polymorphism and extensible records/cases in
our DSL, Lucid, for describing financial products and trading
strategies.  We have also begun to utilize row-polymorphism for
effects tracking, as Lucid makes heavy use of side-effects for user
convenience.  The implementation of row-polymorphism we arrived at
uses simple “lacks constraints” on row type variables and can be
easily retrofitted into the basic Hindley-Milner inferencer, Algorithm
W.  I’ve posted some example toy implementations on github for both
records and effect-tracking [here][#code].

\
![duck typing done right](/img/lego-duck.png)\
\


* * * * * * * *

[#slides]: http://www.timphilipwilliams.com/slides/StructuralTypingForStructuredProducts.pdf
[#code]: https://github.com/willtim/row-polymorphism
