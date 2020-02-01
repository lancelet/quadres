{-|
Module      : Main
Description : Test suite main runner.
-}
module Main
  ( main
  )
where

import qualified Hedgehog
import qualified Test.DocTest

import qualified QuadRES.ParserTest             ( tests )
import qualified QuadRES.RESTest                ( tests )

main :: IO ()
main = do
  runHedgehogTests
  runDocTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "\n---- Running Hedgehog Tests ----"
  mapM_ Hedgehog.checkParallel hedgehogTests
  putStrLn "---- Running Hedgehog Tests ----"

hedgehogTests :: [Hedgehog.Group]
hedgehogTests = [QuadRES.ParserTest.tests, QuadRES.RESTest.tests]

runDocTests :: IO ()
runDocTests = do
  putStrLn "\n---- Running DocTests ----"
  docTests
  putStrLn "---- Finished DocTests ----"

docTests :: IO ()
docTests =
  Test.DocTest.doctest ["-isrc", "src/QuadRES/Parser.hs", "src/QuadRES/RES.hs"]
