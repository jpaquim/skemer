# skemer - a Scheme interpreter written in Haskell

This project aims to update the [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) codebase to current standards, enhance its readability and documentation, and eventually extend its functionality to further cover the Scheme standard.

It will serve mostly as a personal project for learning both Haskell and Scheme.

In terms of coding standards, this project will mostly follow [Johan Tibell's Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

To-do:

- [ ] restructure the code into multiple files
- [ ] replace the deprecated Error monad with Except
- [ ] use qualified imports for external modules
- [ ] compile without warnings
- [ ] thoroughly document code
- [ ] enable comments in scheme files
- [ ] define a consistent split between functionality built into the Haskell interpreter, and Scheme library code
- [ ] implement some of the R7RS standard
- [ ] hygienic macros
- [ ] continuations
- [ ] tail-call optimization
