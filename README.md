# monads-from-scratch

This repo defines some of the most used monads in haskell from scratch, skipping some mathematical concepts, in order 
to understand this useful algebraic constructs.

Please check the tests to see examples of how to use them:

- [Maybe](test/Monad/MaybeSpec.hs)
- [Either](test/Monad/EitherSpec.hs)
- [IO](test/Monad/IOSpec.hs)
- [List](test/Monad/ListSpec.hs)
- [Reader](test/Monad/ReaderSpec.hs)
- [Writer](test/Monad/WriterSpec.hs)
- [State](test/Monad/StateSpec.hs)

And here the implementations: [TypeClasses](src) & [Monads](src/Monad)

## Build
```shell
stack build
```

## Run tests
```shell
stack test
```
