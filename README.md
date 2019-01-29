# light2d-hs

[![Build Status](https://travis-ci.com/PragmaTwice/light2d-hs.svg?branch=master)](https://travis-ci.com/PragmaTwice/light2d-hs)

learn [miloyip/light2d](https://github.com/miloyip/light2d), and translate to haskell

## dependencies

- GHC
- Stack *(for package management)*
- [JuicyPixels](https://github.com/Twinside/Juicy.Pixels) *(for dumping PNG image file)*
- other packages in `package.yaml/dependencies`

## online demo

[TravisCI](https://travis-ci.com/) will automatically run all of these algorithms, render images, and generate a image website using [thumbsup](https://github.com/thumbsup/thumbsup), then upload the site to [Surge](https://surge.sh/).

So enjoy in [light2d-hs.surge.sh](https://light2d-hs.surge.sh/).

## others

#### About random algorithm: 
NO. Random algorithms in Haskell lead to RealWorld operations (mostly when initializing the random seed via `/dev/urandom` on Unix-like systems or `RtlGenRandom` on Windows), and produce lots of IO Monad, which would damage these concise light2d algorithms.

#### About issue for usage, improvement or disagreement:
Welcome.

#### About runtime performance:
Bad, I know it runs very slowly. But I still cannot be skilled with Haskell, so further optimization is unreachable now. Therefore if there are some PRs about these improvement, I will be grateful.
