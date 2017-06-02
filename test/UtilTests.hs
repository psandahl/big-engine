module UtilTests
    ( eitherTwo
    , eitherThree
    , clamp
    ) where

import           Test.HUnit

import qualified BigE.Util  as Util

eitherTwo :: Assertion
eitherTwo = do
    Right (1, 2) @=?
        Util.eitherTwo (r1, r2)

    Left "baarf" @=?
        Util.eitherTwo (l1, r2)

    Left "baarf" @=?
        Util.eitherTwo (r1, l1)

    Left "baarf" @=?
        Util.eitherTwo (l1, l2)

eitherThree :: Assertion
eitherThree = do
    Right (1, 2, 3) @=?
        Util.eitherThree (r1, r2, r3)

    Left "baarf" @=?
        Util.eitherThree (l1, r2, r3)

    Left "baarf" @=?
        Util.eitherThree (r1, l1, r3)

    Left "baarf" @=?
        Util.eitherThree (r1, r2, l1)

    Left "baarf" @=?
        Util.eitherThree (l1, l2, l3)

clamp :: Assertion
clamp = do
    (1 :: Int) @=? Util.clamp 1 10 0
    (1 :: Int) @=? Util.clamp 1 10 1
    (10 :: Int) @=? Util.clamp 1 10 10
    (10 :: Int) @=? Util.clamp 1 10 11

r1 :: Either String Int
r1 = Right 1

r2 :: Either String Int
r2 = Right 2

r3 :: Either String Int
r3 = Right 3

l1 :: Either String Int
l1 = Left "baarf"

l2 :: Either String Int
l2 = Left "urk"

l3 :: Either String Int
l3 = Left "burk"
