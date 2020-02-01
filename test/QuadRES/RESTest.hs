{-|
Module      : QuadRES.RESTest
Description : Tests for the QuadRES.RES module.
-}
{-# LANGUAGE OverloadedStrings #-}
module QuadRES.RESTest
    ( tests
    )
where

import qualified Hedgehog                      as H
import           Hedgehog                       ( (===) )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import qualified QuadRES.RES                   as RES

tests :: H.Group
tests = H.Group "QuadRES.RES"
                [("mkRealN and realNDigits round-trip", prop_mkRealN_trip)]

prop_mkRealN_trip :: H.Property
prop_mkRealN_trip = H.property $ do
    ones       <- H.forAll $ Gen.word8 $ Range.linear 0 9
    tenths     <- H.forAll $ Gen.word8 $ Range.linear 0 9
    hundredths <- H.forAll $ Gen.word8 $ Range.linear 0 9
    let (o', t', h') = RES.realNDigits (RES.mkRealN ones tenths hundredths)
    o' === ones
    t' === tenths
    h' === hundredths
