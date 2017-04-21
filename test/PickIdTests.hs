module PickIdTests
    ( zeroValuedPickId
    , incresedPickId
    , pickIdConstruction
    ) where

import           Test.HUnit

import           BigE.MousePicker (literalPickId, nextPickId, zeroPickId)

zeroValuedPickId :: Assertion
zeroValuedPickId =
    literalPickId 0 0 0 @=? zeroPickId

incresedPickId :: Assertion
incresedPickId =
    literalPickId 0 0 1 @=? nextPickId zeroPickId

pickIdConstruction :: Assertion
pickIdConstruction = do
    literalPickId 0 1 0   @=? nextPickId (literalPickId 0 0 255)
    literalPickId 0 255 1 @=? nextPickId (literalPickId 0 255 0)
    literalPickId 1 0 0   @=? nextPickId (literalPickId 0 255 255)
