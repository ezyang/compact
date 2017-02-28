# compact

[![Hackage version](https://img.shields.io/hackage/v/compact.svg?label=Hackage)](https://hackage.haskell.org/package/compact)

*Non-GC'd, contiguous storage for immutable data structures.*

This package provides user-facing APIs for working with "compact regions", which
hold a fully evaluated Haskell object graph.  These regions maintain the
invariant that no pointers live inside the struct that point outside it, which
ensures efficient garbage collection without ever reading the structure contents
(effectively, it works as a manually managed "oldest generation" which is never
freed until the whole is released).

When would you want to use a compact region? The simplest use case is this: you
have some extremely large, long-lived, pointer data structure which GHC has
uselessly been tracing when you have a major collection. If you place this
structure in a compact region, after the initial cost of copying the data into
the region, you should see a speedup in your major GC runs.

This package is currently highly experimental, but we hope it may be useful to
some people.  It is GHC 8.2 only.  The bare-bones library that ships with GHC is
ghc-compact.

## Quick start

* Import `Data.Compact`

* Put some data in a compact region with `compact :: a -> IO (Compact a)`,
  e.g., `cr <- compact someBigDataStructure`, fully evaluating it in
  the process.

* Use `getCompact :: Compact a -> a` to get a pointer inside the region,
  e.g., `operateOnDataStructure (getCompact cr)`.  The data pointed to
  by these pointers will not participate in GC.

* Import `Data.Compact.Serialize` to write and read compact regions from files.

## Tutorial

**Garbage collection savings.** It's a little difficult to construct a
compelling, small example showing the benefit, but here is a very simple case
from the `nofib` test suite, the `spellcheck` program.  `spellcheck` is a very
simple program which reads a dictionary into a set, and then tests an input
word-by-word to see if it is in the set or not (yes, it is a *very* simple
spell checker):

```
import System.Environment (getArgs)
import qualified Data.Set as Set
import System.IO

main = do
  [file1,file2] <- getArgs
  dict <- readFileLatin1 file1
  input <- readFileLatin1 file2
  let set = Set.fromList (words dict)
  let tocheck = words input
  print (filter (`Set.notMember` set) tocheck)

readFileLatin1 f = do
  h <- openFile f ReadMode
  hSetEncoding h latin1
  hGetContents h
```

Converting this program to use a compact region on the dictionary is very
simple: add `import Data.Compact`, and convert `let set = Set.fromList (words
dict)` to read `set <- fmap getCompact (compact (Set.fromList (words dict)))`:

```
import System.Environment (getArgs)
import qualified Data.Set as Set
import System.IO
import Data.Compact -- **

main = do
  [file1,file2] <- getArgs
  dict <- readFileLatin1 file1
  input <- readFileLatin1 file2
  set <- fmap getCompact (compact (Set.fromList (words dict))) -- ***
  let tocheck = words input
  print (filter (`Set.notMember` set) tocheck)

readFileLatin1 f = do
  h <- openFile f ReadMode
  hSetEncoding h latin1
  hGetContents h
```

Breaking down the new line: `compact` takes an argument `a` which must be pure
and immutable and then copies it into a compact region. This function returns a
`Compact a` pointer, which is simultaneously a handle to the compact region as
well as the data you copied into it.  You get back the actual `a` data that
lives in the region using `getCompact`.

Using the sample `nofib` input
([words](https://github.com/ghc/nofib/blob/master/gc/spellcheck/words) and
[input](https://github.com/ghc/nofib/blob/master/gc/spellcheck/input>)), we can take
a look at our GC stats before and after the change.  To make the effect more
pronounced, I've reduced the allocation area size to 256K, so that we do more
major collections.  Here are the stats with the original:

```
   1,606,462,200 bytes allocated in the heap
     727,499,032 bytes copied during GC
      24,050,160 bytes maximum residency (21 sample(s))
         107,144 bytes maximum slop
              71 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      6119 colls,     0 par    0.743s   0.754s     0.0001s    0.0023s
  Gen  1        21 colls,     0 par    0.608s   0.611s     0.0291s    0.0582s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    2.012s  (  2.024s elapsed)
  GC      time    1.350s  (  1.365s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    3.363s  (  3.389s elapsed)

  %GC     time      40.2%  (40.3% elapsed)

  Alloc rate    798,416,807 bytes per MUT second

  Productivity  59.8% of total user, 59.7% of total elapsed
```

Here are the stats with compact regions:

```
   1,630,448,408 bytes allocated in the heap
     488,392,976 bytes copied during GC
      24,104,152 bytes maximum residency (21 sample(s))
          76,144 bytes maximum slop
              55 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      6119 colls,     0 par    0.755s   0.770s     0.0001s    0.0017s
  Gen  1        21 colls,     0 par    0.147s   0.147s     0.0070s    0.0462s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    1.999s  (  2.054s elapsed)
  GC      time    0.902s  (  0.918s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    2.901s  (  2.972s elapsed)

  %GC     time      31.1%  (30.9% elapsed)

  Alloc rate    815,689,434 bytes per MUT second

  Productivity  68.9% of total user, 69.1% of total elapsed
```

You can see that while the version of the program with compact regions allocates
slightly more (since it performs a copy on the set), it copies nearly half as
much data during GC, reducing the time spent in major GCs by a factor of three.
On this particular example, you don't actually save that much time overall
(since the bulk of execution is spent in the mutator)--a reminder that one
should always measure before one optimizes.

**Serializing to disk.**
You can take the data in a compact region and save it to disk, so that you can
load it up at a later point in time.  This functionality is provided by
`Data.Compact.Serialized`: `writeCompact` and `unsafeReadCompact` let you
write a compact to a file, and read it back again:

```
{-# LANGUAGE TypeApplications #-}
import Data.Compact
import Data.Compact.Serialize
main = do
    writeCompact @(String, Bool) "somefile" ("I want to serialize this", True)
    res <- unsafeReadCompact @(String, Bool) "somefile"
    case res of
        Left err -> fail err
        Right c -> print (getCompact c)
```

Compact regions written to handles this way are subject to some
restrictions:

* Our binary representation contains direct pointers to the info
  tables of objects in the region.  This means that the info tables
  of the receiving process must be laid out in exactly the same
  way as from the original process; in practice, this means using
  static linking, using the exact same binary and turning off ASLR.  This
  API does NOT do any safety checking and will probably segfault if you
  get it wrong.  DO NOT run `unsafeReadCompact` on untrusted input.

* You must read out the value at the correct type.  We will
  check this for you and raise an error if the types do not match.
  To tell `unsafeReadCompact` what type it should read out with,
  the `TypeApplications` extension may come in handy (this extension
  is guaranteed to be available, since compact only supports GHC 8.2
  or later!)
