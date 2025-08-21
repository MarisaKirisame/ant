# TODO: break into multiple documents, figure out how code should link to docs
# Partial Memoization

## Background
Incremental Computing aim to speed up computation by reusing previous results. 
Due to its prevalency in computing, Incremental Computation had been applied to various domain, such as Build System, Database, Compiler, Web Browsers.

## Memoization
The most classic method to incrementalize a program is memoization. To memoize a function, one extend the function with an auxilary data structure (typically a hashmap), which pair inputs of the function with its output. On repeated invoation, this memo-table can skip the actual execution and return the recorded output directly.

The key issue with memoization is that it is too coarse grained. The key problem with memoization is spine-traversal. For example, supposed `map f xs`, including all recursive call is memoized. `map f (xs ++ [x])`, however, cannot reuse any previous result, and must recompute from scratch. 

## Insight
To fix this, memoization should not be applied on the whole value. Instead, memoization split the input `A`, which is an CEK machine, into two part: the memoized fragment, `F[]` an CEK machine with open term `FVS`, and the carry-on fragment `XS`, a enviroment binding the open terms. The memoized fragment is lookuped and matched in a data structure. The resulting value in the memoization table is thus a term `B' = G[FVS]` reachable from `A' = F[FVS]` (`A' ->* B'`). Memoization then allow skipping in the form of `A = F[XS] ->* G[XS] -> B` for arbitary `XS`.

As an example, from a bird eye view, we should be memoizing `map f (xs ++ [_0])` to `map f xs ++ map f _0` - in other word, memoizing a prefix of the input list, so the result is usable on a right-extended list. (A left-extended list is already handled by classic memoization).

To sum up, to use a `F[XS] ->* G[XS]` entry to skip computation, we have to:
- split the current program `X` into `A[BS]`, according to some `skeleton`
- check equality between `F` and `A`
- subst `BS` into `G[XS]` to create the final term `Y = G[BS]` such that `X = A[BS] ->* G[BS] = Y`

- X    -split>   F[XS]
- |                |
- |                |
- v                v
- Y    <subst-   G[XS]

Note that there are two class of concepts, the term/enviroment/substitution that operate in object level, and one that operate in the meta level, operating over the whole CEK machine. The former is in lower case while the latter is in upper case.

## CEK Machine
- CEK: a meta-value
- C: represented by a program counter
- E: a list of value
- K: reified to be a value

## Monoid Parsing
- Finger Tree
- Monoid
- Degree
- Max Degree

## Splitting
- Store: a meta-environment
- Reference: meta-variable
- Values in CEK might hold References

## Checking
- Problem: cannot traverse structure, otherwise too expensive
- e.g. need to get prefix of length X in sublinear term
- Solution: Monoid Hashing over a tree that represent the list

## Substitution
- Subst a single value: loop over all references resolving them
- Subst a CEK: note dependency between values, need to resolve backward in store order

## Memo Structure
- Interactive process, a tree of hashmap
- Each node: require a splitting, result in hashmap, include a 'current' CEK in case of no match
- Nodes might also be incomplete - then no hashmap/splitting, only 'current' is available

## Skeleton Synthesis
- Whenever evaluation stuck (due to encountering references):
- We know what is needed, extend the memo tree with that splitting
- Should request fragment length exponential to tree depth, so tree climbing is polylog

## Updating Memo
- Execute 3 steps in cycle to ensure progress in CEK and improvement in memo tree
- Lookup the corresponding CEK/splitting/node by climbing the tree
- Improve the lookuped CEK by skipping/stepping(if skipping failed)
- Use the commuting square to jump forward the main CEK
- `X -> F[XS] -> G[XS] -> H[XS]`
- "split    "
-       "lookup      "
-                "lookup       "
-       "improve               " 
# Old, do not read. TODO: reuse what's good here
### Value Representation
To provide quick hashing of chunks, and to define chunk for algebaric data type (trees), ant use a finger tree of word (fixed size int) to represent a value. 
An integer is represented as a singleton sequence, storing a single word denoting that int, 
and an algebraic data type is represented by a sequence where the head value is a unique constructor tag, and all fields representation are joined without any separators.
Both values in the environment and the value representing continuation is denoted this way. 

The finger tree is annotated with word lengths, degree/max degree (from monoid parsing), and monoid hashing. This allow the finger tree to be used as a sequence of words or as an ADT, selecting arbitary fragment/child, and maintaining a hash along the way.

A degree is an integer which roughly denote how many values the sequence of word store.
Thus, an integer have an degree of 1, and a constructor of n field have a degree of (1-n).
This means that a constructor, alongside its n fields, made up 1 value.

Due to not having separators, this degree/max-degree is essential for seclecting children down the tree: 
the nth value start after the earliest position with max degree n.

### Memo Structure
The memo structure denote an interactive process. It is a tree, where each branch node contain a fetch request, requiring the state machine to read n words from location l, which is either a marker denoting K, or an (int index) into E, alongside a offset (which skip past the initial n values). 

The branch node also contain a hashtable where the key is the fetch result, and the value is next node. Ant climb this tree until leaf is reached, or until the fetch result is not in hashtable. In either case, ant will execute a jump function which fast forward the CEK machine, skipping intermedite computations.

Note that the memo tree might request to fetch different parts of a value. 
To support this, each time a fetch occur, the leftover part is inserted into a match log (which is a sequence of value). 
A location can then additionally be an index into the match log with the offset.

### Recording new entries
Once ant had selected and hashed a fragment, ant will enter recording mode, saving the old cek machine into history, and creating a new one. 
The cek machine also store a pointer to a memo structure node, so the recording result can be inserted back into it cheaply.

To track whether words are inside/outside of the fragment, the finger tree may additionally contain reference, a location into the history, alongside the monoidal parsing representation (so a value with reference can still be used). 

When ant will execute past the fragment (so attempting to read references as words), the recording is done. 
The reference can then be unlifted back to values, by replacing them with corresponding values, and this unlifting process is entered into the memo node, 
which will be executed under different history.
In other words, the memo result is a CEK with reference, and jumping forward is implemnted by resolving the references.

Note that it is possible to enter recording mode inside recording mode, so the history form a stack, pushing/popping upon entering/exiting.

### Extension
Ant select the memoized fragment in a demand driven manner - that is, it select values which will/have a high chance to be used.
More specifically whenever a read violation occur (that is, attempting to read a reference), ant is given a location which must be read for progression.

Instead of exiting recording, ant can then extend the fragment with n words starting at that location, where n exponentially increase upon reading the same value (reading a match log count as reading the original value where the log is derived from).

Note that extension mean a reference can now be readable, as the values it point to might be newly extended. 
Ant resolve them on demand via a method similar to path-compression in union find.

Extension may not always succeed - remember that history is a stack, where the readbale fragments form a decreasing subset of all values. 
If extending will break this violation, ant cannot extend and the recording must finish.
