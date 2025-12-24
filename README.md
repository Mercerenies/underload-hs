# underload-hs

`underload-hs` is an interpreter for the [Underload programming
language](https://esolangs.org/wiki/Underload).

There are a lot of interpreters for Underload out there, but they all
use plain strings or lists for the underlying data, which is
suboptimal in a language where concatenation is the most common
operation. `underload-hs` performs the following optimizations:
* Your code is stored as a `Data.Sequence`, i.e. a [finger
  tree](https://en.wikipedia.org/wiki/Finger_tree), for efficient
  concatenation and popping.
* Parenthesized code is stored as a "quoted" term, rather than
  literally concatenating and un-concatenating parentheses all the
  time.

With these small optimizations alone, I've observed up to five times
faster performance using this interpreter compared to the other ones
on the market right now.

## Running

This is a Haskell Stack project.

```
stack build
stack run <underload-filename>
```

Some assorted example files are available in `examples/`.
