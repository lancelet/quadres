{-# LANGUAGE OverloadedStrings #-}
module QuadRES.Parser where

import           Control.Monad.Identity         ( Identity )
import           Data.Char                      ( ord )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Data.Word                      ( Word8 )
import qualified Text.Megaparsec               as MP
import           Text.Megaparsec                ( (<?>)
                                                , (<|>)
                                                )

import qualified QuadRES.RES                   as RES

type Parsec e s a = MP.ParsecT e s Identity a

type Parser a = Parsec Void Text a

-- | Parse a color.
--
-- >>> MP.parseMaybe pColor "blue"
-- Just Blue
pColor :: Parser RES.Color
pColor =
  (MP.chunk "black" $> RES.Black)
    <|> (MP.chunk "red" $> RES.Red)
    <|> (MP.chunk "green" $> RES.Green)
    <|> (MP.chunk "blue" $> RES.Blue)
    <|> (MP.chunk "white" $> RES.White)
    <|> (MP.chunk "aqua" $> RES.Aqua)
    <|> (MP.chunk "fuchsia" $> RES.Fuchsia)
    <|> (MP.chunk "gray" $> RES.Gray)
    <|> (MP.chunk "lime" $> RES.Lime)
    <|> (MP.chunk "maroon" $> RES.Maroon)
    <|> (MP.chunk "navy" $> RES.Navy)
    <|> (MP.chunk "olive" $> RES.Olive)
    <|> (MP.chunk "purple" $> RES.Purple)
    <|> (MP.chunk "silver" $> RES.Silver)
    <|> (MP.chunk "teal" $> RES.Teal)
    <|> (MP.chunk "yellow" $> RES.Yellow)

---- Auxiliary Definitions

-- | Parse a real number, in the range '0.00' to '9.99'.
--
-- This accepts numbers with two decimal digits:
-- >>> MP.parseMaybe pRealN "4.32"
-- Just 4.32
--
-- We can also omit the decimal dot:
-- >>> MP.parseMaybe pRealN "6"
-- Just 6.00
--
-- The digit before the dot can be omitted:
-- >>> MP.parseMaybe pRealN ".2"
-- Just 0.20
-- >>> MP.parseMaybe pRealN ".25"
-- Just 0.25
--
-- But we cannot write a dot without any following digits:
-- >>> MP.parseMaybe pRealN "2."
-- Nothing
--
-- We also cannot include extra digits beyond those allowed:
-- >>> MP.parseMaybe pRealN "42.4"
-- Nothing
-- >>> MP.parseMaybe pRealN "2.445"
-- Nothing
pRealN :: Parser RES.RealN
pRealN = do
  digit1    <- MP.option 0 pDigit
  digit2opt <- MP.optional (MP.single '.' *> pDigit)
  let digit2 = fromMaybe 0 digit2opt
  digit3 <- case digit2opt of
    Just _  -> MP.option 0 pDigit
    Nothing -> pure 0
  pure (RES.mkRealN digit1 digit2 digit3) <?> "Real number"

-- | Parses a single digit into a 'Word8'.
--
-- >>> MP.parseMaybe pDigit "4"
-- Just 4
--
-- >>> MP.parseMaybe pDigit "a"
-- Nothing
pDigit :: Parser Word8
pDigit = (\c -> fromIntegral (ord c - 48)) <$> MP.satisfy isDigit <?> "Digit"

---- Character Classes

-- | True if a character is a non-zero digit '1'-'9'.
--
-- >>> isNonZeroDigit '0'
-- False
--
-- >>> isNonZeroDigit '1'
-- True
isNonZeroDigit :: Char -> Bool
isNonZeroDigit c = c' >= 49 && c' <= 57
 where
  c' :: Int
  c' = ord c

-- | True if a character is a digit '0' - '9'.
--
-- >>> isDigit '4'
-- True
--
-- >>> isDigit 'a'
-- False
isDigit :: Char -> Bool
isDigit c = c' >= 48 && c' <= 57
 where
  c' :: Int
  c' = ord c

-- $setup
-- >>> :set -XOverloadedStrings
