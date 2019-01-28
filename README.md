# light2d-hs

learn miloyip/light2d, and translate to haskell

## dependencies

- Haskell
- Stack *(for package management)*
- JuicyPixels *(for dumping PNG image file)*
- other packages in `package.yaml/dependencies`

## others

#### About random algorithm: 
NO. Random algorithms in Haskell lead to RealWorld operations (mostly when initializing the random seed via `/dev/urandom` on Unix-like systems or `RtlGenRandom` on Windows), and produce lots of IO Monad, which would damage these concise light2d algorithms.

#### About issue for usage, improvement or disagreement:
Welcome.
