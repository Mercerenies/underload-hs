# underload-hs

`underload-hs` is an interpreter for the [Underload programming
language](https://esolangs.org/wiki/Underload).

There are a lot of interpreters for Underload out there, but they all
use plain strings or lists for the underlying data, which is
suboptimal in a language where concatenation is the most common
operation. `underload-hs` performs the following optimizations:

* Your code is stored as a `Data.Sequence`, i.e. a [finger
  tree](https://en.wikipedia.org/wiki/Finger_tree), for efficient
  concatenation and popping. Concatenating code is `O(log(min(N, M)))`
  in the sizes of the arguments, and popping is `O(1)`.
* Parenthesized code is stored as a "quoted" term, rather than
  literally concatenating and un-concatenating parentheses all the
  time. The "enclose" operation `a` is `O(1)` and the "eval" operation
  `^` is `O(log(N))` in the size of the argument.

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

## Supported Commands

`underload-hs` supports all of the standard Underload commands, plus a
couple of extra non-standard extensions useful for debugging.

Note: Stack effects are listed using the [Factor
convention](https://docs.factorcode.org/content/article-effects.html).

Standard? | Command | Name              | Stack Effect       | Notes
----------|---------|-------------------|--------------------|-------------------------------------------------------
✔         | `~`     | Swap              | `( x y -- y x )`   | Swaps the top two stack elements
✔         | `:`     | Duplicate         | `( x -- x x )`     | Duplicates the top stack element
✔         | `!`     | Discard           | `( x -- )`         | Pops and discards the top stack element
✔         | `*`     | Concatenate       | `( x y -- xy )`    | Concatenates the top two stack elements
✔         | `(...)` | Push Literal      | `( -- x )`         | Pushes a literal value onto the stack
✔         | `a`     | Enclose           | `( x -- <x> )`     | Wraps the top stack value in parentheses
✔         | `^`     | Evaluate          | `( ..a x -- ..b )` | Evaluates the top stack element
✔         | `S`     | Output            | `( x -- )`         | Prints the top stack element to the console
✗         | `D`     | Stack&nbsp;Length | `( -- )`           | Prints the current length of the stack to the console
✗         | `d`     | Stack&nbsp;Dump   | `( -- )`           | Prints the stack, in full, to the console



## License

This project is licensed under the [MIT License](LICENSE).

If you use it for something cool, I'd love to hear about it :)
