-- |
-- Module: BigE.Util
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Utility functions.
module BigE.Util
    ( eitherTwo
    , eitherThree
    , eitherFour
    , clamp
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

-- | Utility for evaluating a tuple of four 'Either's. The left type must be
-- same for all of them. In case of multiple errors only the first is
-- reported.
eitherFour :: (Either a b, Either a c, Either a d, Either a e) -> Either a (b, c, d, e)
eitherFour (e1, e2, e3, e4)
    | isRight e1 && isRight e2 && isRight e3 && isRight e4 =
        Right (fromRight e1, fromRight e2, fromRight e3, fromRight e4)
    | isLeft e1 = Left (fromLeft e1)
    | isLeft e2 = Left (fromLeft e2)
    | isLeft e3 = Left (fromLeft e3)
    | otherwise = Left (fromLeft e4)

-- | clamp min max val: clamp the value to fit the range given by min and max.
clamp :: Ord a => a -> a -> a -> a
clamp mn mx = min mx . max mn

-- | The functions below are both partial function, and shall only be used
-- at places where values are guareded with isLeft or isRight.
fromLeft :: Either a b -> a
fromLeft (Left err) = err
fromLeft _          = error "Shall not happen"

fromRight :: Either a b -> b
fromRight (Right value) = value
fromRight _             = error "Shall not happen"
