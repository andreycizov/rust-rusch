# rusch 

Rust scheme parser definitions. I have been following [r7rs](https://small.r7rs.org/attachment/r7rs.pdf) for the
formal syntax, but it shouldn't be too hard to switch to another version.

The [parser](./src/parser/) is defined using nom.

I would like to note that as of right now - if you pick the [Datum parser](./src/parser/datum.rs)
and parse an arbitrary program with it, it should not be too hard to implement a Scheme interpreter
from those data structures.

## Roadmap

### What I am trying to achieve

I am not building a Scheme interpreter, but rather a set of tools to easily transform Scheme
sources and data structures into representations that may be further useful for building
compilers and interpreters. Thus from my side I am ignoring most of the standard library.

The future of this project is to compile the Scheme sources into a simple assembly language
that works directly with contexts (stack frames), which are not a part of standard linear
stack, but rather allocated on the heap.

### Priority

- ~~Implement a non-recursive macro substitutor: given a macro definition `my-macro` and
   a call site of that macro: `(my-macro argA argB)`, resolve that with a single pass of macro invocation.~~
   (implemented in [rusch.eval.macros](./src/eval/macros.rs))
- Implement a [CPS transformer](https://en.wikipedia.org/wiki/Continuation-passing_style) for all call sites of the program. 
    1. at this step we're planning to substitute all macros
- Implement a [lambda lifter](https://en.wikipedia.org/wiki/Lambda_lifting).
- Implement type information pass.
    1. Support strings.
    2. Support function (closure) pointers.
    3. Aggressively raise errors for anything else. 

## Usage

For now, just look in [tests](./tests/).

## License

`rusch` is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in rusch by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
