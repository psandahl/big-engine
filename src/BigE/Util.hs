-- |
-- Module: BigE.Util
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Util
    ( eitherTwo
    , eitherThree
    ) where

import           Data.Either (isLeft, isRight)

-- | Utility for evaluating a tuple of two 'Either's. The left type must be the
-- same for both of them. In case of multiple errors only the first is
-- reported.
eitherTwo :: (Either a b, Either a c) -> Either a (b, c)
eitherTwo (e1, e2)
    | isRight e1 && isRight e2 =
        Right (fromRight e1, fromRight e2)
    | isLeft e1 = Left (fromLeft e1)
    | otherwise = Left (fromLeft e2)

-- | Utility for evaluating a tuple of three 'Either's. The left type must be the
-- same for all of them. In case of multiple errors only the first is
-- reported.
eitherThree :: (Either a b, Either a c, Either a d) -> Either a (b, c, d)
eitherThree (e1, e2, e3)
    | isRight e1 && isRight e2 && isRight e3 =
        Right (fromRight e1, fromRight e2, fromRight e3)
    | isLeft e1 = Left (fromLeft e1)
    | isLeft e2 = Left (fromLeft e2)
    | otherwise = Left (fromLeft e3)

-- | The functions below are both partial function, and shall only be used
-- at places where values are guareded with isLeft or isRight.
fromLeft :: Either a b -> a
fromLeft (Left err) = err
fromLeft _          = error "Shall not happen"

fromRight :: Either a b -> b
fromRight (Right value) = value
fromRight _             = error "Shall not happen"
