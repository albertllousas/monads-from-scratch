# monads-from-scratch

This repository does not pretend to be a production library, it defines some of the most used monads in haskell from 
scratch and examples on how to use them, in order to understand this useful algebraic constructs.

Note: Some mathematical concepts are skipped. 

Please check the tests to see examples of how to use them.

- Maybe: [Implementation](src/Monad/Maybe.hs) & [Usage](test/Monad/MaybeSpec.hs)
- Either: [Implementation](src/Monad/Either.hs) & [Usage](test/Monad/EitherSpec.hs)
- IO: [Implementation](src/Monad/IO.hs) & [Usage](test/Monad/IOSpec.hs)
- List: [Implementation](src/Monad/List.hs) & [Usage](test/Monad/ListSpec.hs)
- Reader: [Implementation](src/Monad/Reader.hs) & [Usage](test/Monad/ReaderSpec.hs)
- Writer: [Implementation](src/Monad/Writer.hs) & [Usage](test/Monad/WriterSpec.hs)
- State: [Implementation](src/Monad/State.hs) & [Usage](test/Monad/StateSpec.hs)

Monad transformers:

- StateT: [Implementation](src/Monad/Transformer/StateT.hs) & [Usage](test/Monad/Transformer/StateTSpec.hs) 

Type classes:

- Functor: [Implementation](src/Functor.hs)
- Applicative Functor: [Implementation](src/ApplicativeFunctor.hs)
- Monad: [Implementation](src/Monad.hs)

## Build
```shell
stack build
```

## Run tests
```shell
stack test
```
