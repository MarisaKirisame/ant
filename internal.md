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
The evaluator that Ant incrementalises is a classic CEK machine.  A CEK state is a triple `(C, E, K)`:

- `C` (Control) is the program-counter for the expression we are currently stepping.  Rather than keeping the AST inline, Ant compiles expressions to a table and uses an integer program counter so that states are compact and easy to memoise.
- `E` (Environment) is the run-time stack.  We store values in a `Dynarray`, treating it as a contiguous sequence so that we can push, pop, or slice efficiently when we substitute memoised results back in.
- `K` (Continuation) represents “what to do next”.  Instead of a host-language stack frame, continuations are themselves values encoded with the same memo-friendly representation as user data, which allows them to be serialised and compared during memo lookups.

A CEK step rewrites one such triple to another.  Memoisation needs to capture these machine states faithfully: when the memo table says “from this `C` with this `E` and `K` we can skip ahead”, we substitute the matching environment fragments and resume execution at the new control location without re-running the intermediate steps.

## Monoid Parsing
Memoisation requires slicing and comparing pieces of `E` and `K` cheaply.  To do so Ant stores every value as a finger tree whose nodes carry multiple monoid measures:

- **Finger tree backbone.**  Values are sequences of machine words arranged in a finger tree, giving us amortised `O(log n)` splits and concatenations while keeping positional access inexpensive.
- **Monoid measures.**  Each node is labelled with three measures that compose in constant time:
  - a **hash monoid** so we can compare large prefixes without re-reading every word,
  - the **degree** monoid that tracks how many logical values a word-span contributes (constructors may have negative degree to account for their fields),
  - and the **max-degree prefix** monoid that tells us the max degree of all the prefixes.

With these measures we can “parse” the finger tree using only monoid metadata: given an index we binary-search the tree to find the corresponding child, and when we split we get hashes for both halves essentially for free.  This combination of hashing and degree tracking is what makes memo-table lookups scale; we identify matching prefixes and environments without linear scans.

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
### Memo Structure
The memo structure denote an interactive process. It is a tree, where each branch node contain a fetch request, requiring the state machine to read n words from location l, which is either a marker denoting K, or an (int index) into E, alongside a offset (which skip past the initial n values). 

The branch node also contain a hashtable where the key is the fetch result, and the value is next node. Ant climb this tree until leaf is reached, or until the fetch result is not in hashtable. In either case, ant will execute a jump function which fast forward the CEK machine, skipping intermedite computations.

Note that the memo tree might request to fetch different parts of a value. 
To support this, each time a fetch occur, the leftover part is inserted into a match log (which is a sequence of value). 
A location can then additionally be an index into the match log with the offset.
