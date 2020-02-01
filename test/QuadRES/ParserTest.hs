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
import Hedgehog ((===))
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import qualified Data.Char (isDigit)

import qualified QuadRES.Parser                as Parser

tests :: H.Group
tests = H.Group
    "QuadRES.Parser"
    [ ("'0'-'9' are definitely digits"    , unit_IsDigit_0_9)
    , ("isDigit matches Data.Char.isDigit", prop_IsDigit)
    ]

unit_IsDigit_0_9 :: H.Property
unit_IsDigit_0_9 =
    H.withTests 1 $ H.property $ H.assert (all Parser.isDigit ['0' .. '9'])

prop_IsDigit :: H.Property
prop_IsDigit = H.property $ do
    c <- H.forAll Gen.alphaNum
    Data.Char.isDigit c === Parser.isDigit c

