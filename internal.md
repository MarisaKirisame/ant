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

  This representation mean retrieving constructor argument require monoid parsing of reverse polish expression.

  We have a monoidal homomorphism from Words to degree.

  Any basica value have a degree of 1, and any constructor of arity x, have a degree of 1-x. Composing is merely addition.

  Then, giving a finger tree obtained by appending multiple values, the amount of values is equal to the degree.

  To select the nth value of this finger tree, define a `max_degree` which is the max degree of all prefixes of the Word sequence.

  This is also a monoid homomorphism, and the nth value start on the earliest occasion where max_degree = n.

## State Representation
Programs in Ant is compiled into a CEK machine, where C is an int, E is an array of value, and K is a value.

Unlike the classical CEK machine, in Ant, `Apply` is a program state instead of a function, keeping all steps a small finite amount of work.

Memoization in Ant thus memoize a fragment of the state, to skip to a state which cannot transit withou reading outside of the fragment.

## Fragment and Match Log
A matched fragment is a sequence of Words, while the unmatched fragment 
