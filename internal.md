# Prefix Memoization

## Background
Incremental Computing aim to speed up computation by reusing previous results. 
Due to its prevalency in computing, Incremental Computation had been applied to various domain, such as Build System, Database, Compiler, Web Browsers.
An incremental framework/programming language aim to transform non-incremental program into incremental ones.

### Memoization
The most basic method to incrementalize a program is memoization. To memoize a function, one extend the function with an auxilary data structure (typically a hashmap), which pair inputs of the function with its output. On repeated invoation, this memo-table can skip the actual execution and return the recorde output directly.

Memoization requires fast hash/comparison, which can be supplied by hash-consing, or by storing the hash and ignoring hash collision.

A program can be transformed into an incremental one by converting all functions to memoized functions.

The key problem with memoization is spine-traversal. For example, supposed `map f xs`, including all recursive call is memoized. `map f (xs ++ [x])`, however, cannot reuse any previous result, and must recompute from scratch. But, stepping back, the actual solution is clear: `map f (xs ++ [x]) = map f xs ++ [f x]`, thus we should be able to reuse the previous result. (Yes, we still need to traverse the spine in this case, but this is irrelevant: the issue is expensiveness of reuse (which can be fixed in other ways), while previously the issue is not reusing)

### Change Based Approach
To circumvent the spine-traversal problem, change-based approach such as self adjusting computation (SAC) instead maintain a computation graph, updating it whenever the input changed. By doing this, SAC decoupled itself from the shape (spine) of the data, and coupled itself to the computation.

This does solve the spine-traversal problem, but the coupling to compute intoduce other problems.

In particular, there are three kinds of repetition a program may encounter:

- repetition in data: a single value might have repeated subterms. for example, a loop-unrolled function will contain the same instruction repeated multiple times.
- repetition across invocations: multiple invocations to a function share repeated subterms. for example, an analyze pass will be call in a compiler multiple times, each time on similar programs.
- repetition across program invocation: the program (e.g. the compiler) may be called on similar input.

SAC only handle the last repetition, while memoization based approach have no trouble handling all kinds of reptition.

Note: while the last two seems similar, the key difference is externality. SAC split the program into two part: a functional core, and an imperative driver. Incrementalization only occur between the boundary, thus internal invocations are not incremental.
