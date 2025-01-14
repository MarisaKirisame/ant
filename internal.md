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

  One might be tempted to fix this by memoizing a part of the input (instead of the whole input) with trie, however now matching fragment become linear again.

Ant identified this two issues and strive to achieve provide a memoization which is both **fast** and **general**.

