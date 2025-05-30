# Partial Memoization

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

## Solution
The key observation is that both `map f xs` and `map f (xs ++ [x])` share a long and common prefix. 
On this prefix the computation is the same, thus we should not be memoizing values, but chunks of values, which we call fragment.
As chunks are incomplete, we cannot return the corresponding values, but must instead jump to the intermediate state, where the next small-step evaluation will require reading outside of the memoized chunks.

### Sketch
Partial memoization operate on a CEK machine, modified to provide:

- Selection of fragment
- Hashing of chunks
- Recording jump into memo structure
- Applying jump from memo structure

With these features, ant select a fragment and calculate its hash, then start recording the execution under the fragment, until pending out-of-fragment reads,
and record the fragment. Whenever applicable (hash match), ant will jump ahead to reuse old computation.

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
