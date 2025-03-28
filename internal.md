# Ant
Ant is a incremental programming language, which provide **fast** and **general** memoization.

## Memoization
Memoization is a technique that record a fragment of the program state (alongside the execution result), and later upon encountering the same fragment, jump directly to the execution result.

From this definition, immediately we have two critical metric which we want to optimize and compare against:
- How fast is it to match a fragment?

  A naive memo algorithm, e.g. Python's  `functools.lru_cache`, need to traverse the input to index into a hashmap/BST/trie. 

  This mean invoking the memoized function require traversing the whole input, often multiple time, which is unacceptable given lots of function run in linear time with a small constant factor.

  A solution against this is to use hash-consing, or to pair each cons cell with a hash value. Either way, memo lookup is now in constant/log time. But - 
- How general is the fragments?

  Memo algorithm typically memo and match the whole input argument, which is too coarse-grained for any serious use.

  For example, invalidating `map f x`, by inserting into the middle of the list, require a linear traversal of half of the list to fix it.

Ant identified this two issues and strive to achieve provide a memoization which is both **fast** and **general**.

## Value Representation
To provide fast and general memoization, Ant use a finger tree of tagged Word(8-bytes) to represent value. Part of the Word is stolen for tagging purpose.

- A basic value of less then a Word, e.g. an int, can be represented using one Word, using the remaining unstolen bit. The corresponding finger tree thus contain one element.

- An adt is represented as a finger tree of possibly multiple Word. 

  The head byte contain only the constructor tag and nothing else, while the rest of the finger tree represent the arguments of the constructor appended into one finger tree (note: without any separators).

  This representation mean retrieving constructor argument require monoid parsing of polish expression.

  We have a monoidal homomorphism from Words to degree.

  Any basica value have a degree of 1, and any constructor of arity x, have a degree of 1-x. Composing is merely addition.

  Then, giving a finger tree obtained by appending multiple values, the amount of values is equal to the degree.

  To select the nth value of this finger tree, define a `max_degree` which is the max degree of all prefixes of the Word sequence.

  This is also a monoid homomorphism, and the nth value start on the earliest occasion where max_degree = n.

  Note that a key consequence of using polish expression, is that any string can be decomposed into a sequence of n values,
  alongside a possibly empty prefix of degree 1-x, which create a value after consuming n values.

  Formally speaking a string of n `max_degree` and n-x `degree` is n values, followed by a prefix which require x+1 values to complete.

## State Representation
Programs in Ant is compiled into a CEK machine, where C is an int, E is an array of value, and K is a value.

Unlike the classical CEK machine, in Ant, `Apply` is a program state instead of a function, keeping all steps a small finite amount of work.

Memoization in Ant thus memoize a fragment of the state, to skip to a state which cannot transit withou reading outside of the fragment.

## Memoization Overview
A key property in ant is that the fragment to memoize is dynamic - it depend on what part of the values are being read.

This dynamism is handled via 3 interlocking componenting:
- A Reference type, which enhance the value type with late binding, to resolve match request dynamically. 
- A Store, which resolve Reference into value or a match request (if the Reference refer to value from the unmatched memo input).
- A Memo Tree, which interact with the memo caller to submit match request and skip ahead.

## Reference Type
The Reference type refer to a series of values which can possibly be unmatched.

It consist of a source (an index into the store or the memoing CEK state), an offset to skip n values, and the amount of values it hold.

It's `degree` and `max_degree` is thus its value amount.

The seq type is then a finger tree of either Word or Reference, to track unmatched values.

## Store
The Store give name to possibly unmatched fragment, so it can be refered to (via reference), or matched later.

It is an array where entires are either `unmatched` or `matched`. An unmatched entry contain a value from the memo caller, and a matched entry contain a `normal` value.

No entries contain incomplete values. Formally, their `degree` must equal to their `max_degree` and no substring share the same `max_degree`.

This distinction dictate whether an entries is usable (if it is already memoized), or need to be memoized before becoming usable.

A similar mechanism have to be deployed to the CEK states.

Once matched, the value is converted from a `unmatched` to a `matched` with the matched part being Words, and the other part turning into references, to new entries in the store.

## Memo Tree
The Memo Tree is a Tree where each node have a match request, a transit function, and a hashtable to Memo Tree Node.

To skip fragment, the Memo Tree interact with the memo caller by sending its match request, which is like a reference, containing a unmatched source, an offset, but with a length instead of amount of values. 

The memo caller can then lookup the true value in the source, splitting the previously unmatched source into the prefix/suffix, and the matched in-between. 

The prefix/suffix is appended onto the store, and the in-between is used for lookup in the hashtable.

If the lookup succeed, a new match request will be sent, growing exponentially in length, continuing until a lookup fail.

The latest transit function (a function from CEK state to state) is then applied to finish this sequence of memo lookup.

## Adding new entry
TODO
