# idris-modules
This is the github repo accompanying my final year undergraduate project.

The project was to add a first class module system to Idris 2, a dependently typed proof assistant.
This was implemented as a library, via elaborator reflection, a form of metaprogramming.
The module system implements features akin to signatures and functors from the ML family of languages, but not structures; this is due to Idris currently being unable to inspect and reify term-level values through elaborator reflection.

## Usage
This repo exposes a [pack](https://github.com/stefan-hoeck/idris2-pack) package.
You can import it as you would any other library.
For usage examples, see the tests directory, or read the project's report (once uploaded).
