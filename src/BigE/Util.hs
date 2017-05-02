-- |
-- Module: BigE.Util
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Util where

-- | Utility for evaluating a tuple of two 'Either's. The left type must be the
-- same for both of them. In case of multiple errors only the first is
-- reported.
eitherTwo :: (Either a b, Either a c) -> Either a (b, c)
eitherTwo = undefined
