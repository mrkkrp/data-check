# Data Check

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/data-check.svg?style=flat)](https://hackage.haskell.org/package/data-check)
[![Stackage Nightly](http://stackage.org/package/data-check/badge/nightly)](http://stackage.org/nightly/package/data-check)
[![Stackage LTS](http://stackage.org/package/data-check/badge/lts)](http://stackage.org/lts/package/data-check)
[![Build Status](https://travis-ci.org/mrkkrp/data-check.svg?branch=master)](https://travis-ci.org/mrkkrp/data-check)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/data-check/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/data-check?branch=master)

This is a library that implements generalized approach to checking and
verification of data, it's useful, for example, for validation of fields on
web forms.

Typically, there are a number of transformations and checks you may want to
perform on a particular type of data, such as text. Thus, it makes sense to
create all those transformations and checks once and then combine them to
get more complex validators that may vary on per-field basis.

Certainly, if we can normalize and validate, we should normalize first.
However, if we have many normalizing operations, we need a way to specify in
which order they should be performed, or result can be unpredictable.

To specify order in which transformations are performed, `normalizer` and
`normalizerM` functions take a “priority” argument, which is just a
`Natural` number. The bigger the number, the later the function will be
applied, so the transformation with priority 0 will always run first.

This method applies to validators just as well. It's possible to create a
vocabulary of validators that can be mixed together and the result will
be always deterministic.

To support more real-world use cases, every check can be performed inside of
a monad, allowing to query a database for example.

One last thing to note is that every normalizer and every validator should
have a unique priority number. Normalizers (and validators) with the same
priority will overwrite each other. This is by design. Note that normalizer
won't override validator with the same priority though, their
priority-spaces are separate.

## License

Copyright © 2016 Mark Karpov

Distributed under BSD 3 clause license.
