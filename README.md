# TexMixFix
Parse mixfix operators with a directed acyclic precedence graph

How to use this code
--------------------

The most important function is `expression`: this takes a base value and a precedence graph and makes a parser for expressions involving the operators and the base value.

To make a precedence graph of operators, you start with the `emptyGraph` of a list of operators and add edges with the `|+` and `+->` operators (for example, you could write `emptyGraph [times, plus, parens] |+ plus +-> parens |+ times +-> plus |+ times +-> parens`).

The base value parser should match all parts of an expression that are not themselves expressions, such as variables, numbers or strings.

A summary of mixfix
-------------------

Operators consist of name parts and holes: holes (denoted `_`) can be filled with (certain types of) other expressions, and name parts are the required literal parts. For example, the mathematical plus operator can be written as `_+_`, with two holes and a name part with one literal: `["+"]`. Operators all have holes between their name parts, and operators with only holes between the parts are called closed. Operators with a hole before the first name part are postfix operators (such as the element access operator `_[_]`). Operators with a hole after the last name part are prefix operators (such as the `if_then_else_` operator in Haskell), and operators with holes before the first and after the last name parts are called infix operators (such as the plus operator `_+_`).

Infix operators are ambiguous when written one after another, so we distinguish the associativity: if `_*_*_` means `_*(_*_)`, this is a right-associative operator, if it means `(_*_)*_,` this is a left-associative operator`, and if it is simply not allowed, it is a non-associative operator. (If `(_*_)*_ == _*(_*_)`, then we call it simply associative, and we can freely choose between left- or right-association.)

Another form of ambiguity occurs when you have operators with holes at their edges: does `_+_*_` mean `(_+_)*_` or `_+(_*_)`. In the last case, we say `_*_` has a higher precedence than `_+_`. In this parser, `a` has higher precedence than `b` if there is an edge from `b` to `a` in the graph.

Example code
------------

This program includes a couple of example operators, namely a prefix `if_then_else_`, a left- and right-associative `_+_`, a postfix `_[_]` and a closed `(_)` operator. These operators are included in the example graph `predGraph`.

The `$$` operator gives all parse trees that exactly match the given string, for example `expression value predGraph $$ "(if_+_then_[_]else_)+_[_]"`
