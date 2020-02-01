{-|
Module      : QuadRES.ParserTest
Description : Tests for the QuadRES.Parser module.
-}
{-# LANGUAGE OverloadedStrings #-}
module QuadRES.ParserTest
  ( tests
  )
where

import qualified Hedgehog                      as H
import           Hedgehog                       ( (===) )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import qualified Data.Char                      ( isDigit )
import qualified Data.Text                     as Text
import qualified Text.Megaparsec               as MP

import qualified QuadRES.Parser                as Parser
import qualified QuadRES.RES                   as RES

tests :: H.Group
tests = H.Group
  "QuadRES.Parser"
  [ ("pRealN parses shown RealN values"      , prop_pRealN_trip)
  , ("'1'-'9' are definitely non-zero digits", unit_IsNonZeroDigit_1_9)
  , ( "isNonZeroDigit matches Data.Char.isDigit with zero exclusion"
    , prop_IsNonZeroDigit
    )
  , ("'0'-'9' are definitely digits"    , unit_IsDigit_0_9)
  , ("isDigit matches Data.Char.isDigit", prop_IsDigit)
  ]

prop_pRealN_trip :: H.Property
prop_pRealN_trip = H.property $ do
  let genDigit = Gen.word8 (Range.linear 0 9)
  d1 <- H.forAll genDigit
  d2 <- H.forAll genDigit
  d3 <- H.forAll genDigit
  let realN = RES.mkRealN d1 d2 d3
  H.tripping realN (Text.pack . show) (MP.parseMaybe Parser.pRealN)

unit_IsNonZeroDigit_1_9 :: H.Property
unit_IsNonZeroDigit_1_9 =
  H.withTests 1 $ H.property $ H.assert (all Parser.isNonZeroDigit ['1' .. '9'])

prop_IsNonZeroDigit :: H.Property
prop_IsNonZeroDigit = H.property $ do
  c <- H.forAll Gen.alphaNum
  (Data.Char.isDigit c && c /= '0') === Parser.isNonZeroDigit c

unit_IsDigit_0_9 :: H.Property
unit_IsDigit_0_9 =
  H.withTests 1 $ H.property $ H.assert (all Parser.isDigit ['0' .. '9'])

prop_IsDigit :: H.Property
prop_IsDigit = H.property $ do
  c <- H.forAll Gen.alphaNum
  Data.Char.isDigit c === Parser.isDigit c

