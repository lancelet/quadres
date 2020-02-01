{-|
Module      : Main
Description : Test suite main runner.
-}
module Main
    ( main
    )
where

import qualified Test.DocTest

main :: IO ()
main = do
    runHedgehogTests
    runDocTests

runHedgehogTests :: IO ()
runHedgehogTests = pure ()  -- TODO: implement Hedgehog tests

runDocTests :: IO ()
runDocTests = do
    putStrLn "\n---- Running DocTests ----"
    docTests
    putStrLn "---- Finished DocTests ----"

docTests :: IO ()
docTests = Test.DocTest.doctest ["-isrc", "src/QuadRES/Parser.hs"]
