{-# LANGUAGE OverloadedStrings #-}
module QuadRES.Parser where

import qualified Control.Applicative.Combinators.NonEmpty
                                               as NonEmpty
import           Control.Monad.Identity         ( Identity )
import           Data.Char                      ( ord )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )
import           Data.Word                      ( Word8
                                                , Word16
                                                )
import qualified Text.Megaparsec               as MP
import           Text.Megaparsec                ( (<?>)
                                                , (<|>)
                                                )

import qualified QuadRES.RES                   as RES
import qualified QuadRES.Mnemonics             as Mnemonics

type Parsec e s a = MP.ParsecT e s Identity a

type Parser a = Parsec Void Text a

-- | Parse a 'RES.GlyphID'.
--
-- >>> MP.parseMaybe pGlyphID "G14"
-- Just (GlyphIDGardiner "G" 14 Nothing)
--
-- >>> MP.parseMaybe pGlyphID "W24a"
-- Just (GlyphIDGardiner "W" 24 (Just 'a'))
--
-- >>> MP.parseMaybe pGlyphID "open"
-- Just GlyphIDOpen
--
-- >>> MP.parseMaybe pGlyphID "close"
-- Just GlyphIDClose
--
-- >>> MP.parseMaybe pGlyphID "\"[\""
-- Just (GlyphIDShortString '[')
--
-- >>> MP.parseMaybe pGlyphID "rxyt"
-- Just (GlyphIDMnemonic "rxyt")
--
-- >>> MP.parseMaybe pGlyphID "rxytfoo"
-- Nothing
pGlyphID :: Parser RES.GlyphID
pGlyphID =
    (   RES.GlyphIDGardiner
        <$> pCategory
        <*> pNonZeroNat
        <*> MP.optional pLowerLetter
        )
        <|> (MP.chunk "open" $> RES.GlyphIDOpen)
        <|> (MP.chunk "close" $> RES.GlyphIDClose)
        <|> (RES.GlyphIDShortString <$> pShortString)
        <|> (RES.GlyphIDMnemonic <$> pMnemonic)
  where
    pCategory :: Parser Text
    pCategory =
        (Text.singleton <$> MP.satisfy (isCharInRange 'A' 'I'))
            <|> (Text.singleton <$> MP.satisfy (isCharInRange 'K' 'Z'))
            <|> MP.chunk "Aa"
            <|> MP.chunk "NL"
            <|> MP.chunk "NU"

-- | Parse a mnemonic.
--
-- The mnemonic must be in the allowed list.
--
-- >>> MP.parseMaybe pMnemonic "wDAt"
-- Just "wDAt"
--
-- >>> MP.parseMaybe pMnemonic "wDAtf"
-- Nothing
pMnemonic :: Parser Text
pMnemonic = pMnemonic' (Map.keysSet Mnemonics.mnemonics)
  where
    pMnemonic' :: Set Text -> Parser Text
    pMnemonic' nameSet = do
        candidate <- MP.takeWhile1P (Just "Mnemonic Character") isMnemonicChar
        if Set.member candidate nameSet
            then pure candidate
            else MP.failure Nothing Set.empty

-- | Parse whitespace and optional switches.
ws :: Parser RES.Switches
ws = pWhitespace *> pSwitches

-- | Parse a 'RES.Switches'.
--
-- >>> sw = "![red] ![noshade,sep=4.3]"
-- >>> MP.parseMaybe pSwitches sw
-- Just (Switches [Switch [SwColor Red],Switch [SwShadeState NoShade,SwSep (Sep 4.30)]])
pSwitches :: Parser RES.Switches
pSwitches = RES.Switches <$> MP.many pSwitch

-- | Parse a 'RES.Switch'.
--
-- >>> sw = "![blue, noshade, sep=1.2]"
-- >>> MP.parseMaybe pSwitch sw
-- Just (Switch [SwColor Blue,SwShadeState NoShade,SwSep (Sep 1.20)])
--
-- A plain exclamation point can also be a switch. This has no interpretation
-- other than as a possible marker to a human reader:
-- >>> MP.parseMaybe pSwitch "!"
-- Just (Switch [])
pSwitch :: Parser RES.Switch
pSwitch =
    RES.Switch
        <$> (  MP.single '!'
            *> MP.option [] (pBracketedList pSwitchArg)
            <* pWhitespace
            )

-- | Parse a 'RES.SwitchArg'.
--
-- >>> MP.parseMaybe pSwitchArg "green"
-- Just (SwColor Green)
pSwitchArg :: Parser RES.SwitchArg
pSwitchArg =
    (RES.SwColor <$> pColor)
        <|> (RES.SwShadeState <$> pShadeState)
        <|> (RES.SwSep <$> pSep)
        <|> (RES.SwFitState <$> pFitState)
        <|> (RES.SwMirrorState <$> pMirrorState)

-- | Parse a 'RES.FitState'.
--
-- >>> MP.parseMaybe pFitState "fit"
-- Just Fit
--
-- >>> MP.parseMaybe pFitState "nofit"
-- Just NoFit
pFitState :: Parser RES.FitState
pFitState =
    ((MP.chunk "fit" $> RES.Fit) <|> (MP.chunk "nofit" $> RES.NoFit))
        <?> "(fit|nofit)"

-- | Parse a 'RES.MirrorState'.
--
-- >>> MP.parseMaybe pMirrorState "mirror"
-- Just Mirror
--
-- >>> MP.parseMaybe pMirrorState "nomirror"
-- Just NoMirror
pMirrorState :: Parser RES.MirrorState
pMirrorState =
    ((MP.chunk "mirror" $> RES.Mirror) <|> (MP.chunk "nomirror" $> RES.NoMirror)
        )
        <?> "(mirror|nomirror)"

-- | Parse a 'RES.ShadeState'.
--
-- >>> MP.parseMaybe pShadeState "shade"
-- Just Shade
--
-- >>> MP.parseMaybe pShadeState "noshade"
-- Just NoShade
pShadeState :: Parser RES.ShadeState
pShadeState =
    ((MP.chunk "shade" $> RES.Shade) <|> (MP.chunk "noshade" $> RES.NoShade))
        <?> "(shade|noshade)"

-- | Parse a 'RES.Sep'.
--
-- >>> MP.parseMaybe pSep "sep=1.2"
-- Just (Sep 1.20)
--
-- >>> MP.parseMaybe pSep "sep = 1.2"
-- Nothing
pSep :: Parser RES.Sep
pSep = RES.Sep <$> (MP.chunk "sep=" *> pRealN) <?> "sep=<real>"

-- | Parse a 'RES.Color'.
--
-- >>> MP.parseMaybe pColor "blue"
-- Just Blue
pColor :: Parser RES.Color
pColor =
    (   (MP.chunk "black" $> RES.Black)
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
        )
        <?> "Color"

-- | Parse a 'RES.ShadePattern'.
--
-- >>> MP.parseMaybe pShadePattern "ts"
-- Just (ShadePattern (SideT :| [SideS]))
pShadePattern :: Parser RES.ShadePattern
pShadePattern = (RES.ShadePattern <$> NonEmpty.some pSide) <?> "Shade pattern"

-- | Parse a 'RES.Side' to shade.
--
-- >>> MP.parseMaybe pSide "s"
-- Just SideS
pSide :: Parser RES.Side
pSide =
    (   (MP.single 't' $> RES.SideT)
        <|> (MP.single 'b' $> RES.SideB)
        <|> (MP.single 's' $> RES.SideS)
        <|> (MP.single 'e' $> RES.SideE)
        )
        <?> "Side (t,b,s,e)"

-- | Parse a square-bracket-terminated, comma-separated list of items.
--
-- >>> MP.parseMaybe (pBracketedList pRealN) "[ ]"
-- Just []
--
-- >>> MP.parseMaybe (pBracketedList pRealN) "[ 1.3 ]"
-- Just [1.30]
--
-- >>> MP.parseMaybe (pBracketedList pRealN) "[4.3,9.5,3.2]"
-- Just [4.30,9.50,3.20]
--
-- >>> MP.parseMaybe (pBracketedList pRealN) "[ 4.3\n  , 9.5\n  , 3.2\n ]"
-- Just [4.30,9.50,3.20]
--
-- >>> MP.parseMaybe (pBracketedList pRealN) "[ 9.1, ]"
-- Nothing
--
-- >>> MP.parseMaybe (pBracketedList pRealN) "[,]"
-- Nothing
pBracketedList :: Parser a -> Parser [a]
pBracketedList pItem = do
    MP.single '[' $> ()
    pWhitespace
    items <- MP.optional (pList pItem)
    MP.single ']' $> ()
    pure (fromMaybe [] items)

-- | Parse a comma-separated, non-empty list of items.
pList :: Parser a -> Parser [a]
pList pItem = do
    pWhitespace
    item <- pItem
    pWhitespace
    optComma <- MP.optional (MP.single ',')
    case optComma of
        Nothing -> pure [item]
        Just _  -> (item :) <$> pList pItem

-- | Gobble zero or more whitespace characters.
pWhitespace :: Parser ()
pWhitespace = MP.takeWhileP (Just "whitespace") isWhitespace $> ()

---- Auxiliary Definitions

-- | Parse a natural number, in the range 0 to 999 inclusive.
--
-- >>> MP.parseMaybe pNat "1"
-- Just 1
--
-- >>> MP.parseMaybe pNat "12"
-- Just 12
--
-- >>> MP.parseMaybe pNat "123"
-- Just 123
--
-- >>> MP.parseMaybe pNat "0"
-- Just 0
--
-- >>> MP.parseMaybe pNat "999"
-- Just 999
--
-- >>> MP.parseMaybe pNat ""
-- Nothing
--
-- >>> MP.parseMaybe pNat "1000"
-- Nothing
pNat :: Parser Word16
pNat = do
    digit1    <- pDigit
    digit2opt <- MP.optional pDigit
    case digit2opt of
        Nothing     -> pure . fromIntegral $ digit1
        Just digit2 -> do
            digit3opt <- MP.optional pDigit
            case digit3opt of
                Nothing ->
                    pure $ 10 * fromIntegral digit1 + fromIntegral digit2
                Just digit3 ->
                    pure
                        $ (100 * fromIntegral digit1)
                        + (10 * fromIntegral digit2)
                        + fromIntegral digit3

-- | Parse a non-zero natural number, in the range 1 to 999 inclusive.
--
-- >>> MP.parseMaybe pNonZeroNat "1"
-- Just 1
--
-- >>> MP.parseMaybe pNonZeroNat "12"
-- Just 12
--
-- >>> MP.parseMaybe pNonZeroNat "123"
-- Just 123
--
-- >>> MP.parseMaybe pNonZeroNat "999"
-- Just 999
--
-- >>> MP.parseMaybe pNonZeroNat "0"
-- Nothing
--
-- >>> MP.parseMaybe pNonZeroNat ""
-- Nothing
--
-- >>> MP.parseMaybe pNonZeroNat "1000"
-- Nothing
pNonZeroNat :: Parser Word16
pNonZeroNat = do
    digit1    <- pNonZeroDigit
    digit2opt <- MP.optional pDigit
    case digit2opt of
        Nothing     -> pure . fromIntegral $ digit1
        Just digit2 -> do
            digit3opt <- MP.optional pDigit
            case digit3opt of
                Nothing ->
                    pure $ 10 * fromIntegral digit1 + fromIntegral digit2
                Just digit3 ->
                    pure
                        $ (100 * fromIntegral digit1)
                        + (10 * fromIntegral digit2)
                        + fromIntegral digit3

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
--
-- We must parse something though:
-- >>> MP.parseMaybe pRealN ""
-- Nothing
pRealN :: Parser RES.RealN
pRealN = do
    digit1opt <- MP.optional pDigit
    digit2opt <- MP.optional (MP.single '.' *> pDigit)
    -- by this point, we can have obtained "x", "x.x" or ".x"; but if both
    -- digit1opt and digit2opt are empty then that's an error case
    case (digit1opt, digit2opt) of
        -- here we parsed nothing so far; so signal an error
        (Nothing, Nothing) -> MP.failure Nothing Set.empty
        -- we parsed just the first digit, but no period and following digit
        (Just d1, Nothing) -> pure (RES.mkRealN d1 0 0)
        -- we passed a period and a following digit so try for a third digit
        (_      , Just d2) -> do
            let d1 = fromMaybe 0 digit1opt
            d3 <- MP.option 0 pDigit
            pure (RES.mkRealN d1 d2 d3)

-- | Parses a single digit into a 'Word8'.
--
-- >>> MP.parseMaybe pDigit "4"
-- Just 4
--
-- >>> MP.parseMaybe pDigit "a"
-- Nothing
pDigit :: Parser Word8
pDigit = (\c -> fromIntegral (ord c - 48)) <$> MP.satisfy isDigit <?> "Digit"

-- | Parses a single non-zero digit into a 'Word8'.
--
-- >>> MP.parseMaybe pNonZeroDigit "4"
-- Just 4
--
-- >>> MP.parseMaybe pNonZeroDigit "0"
-- Nothing
pNonZeroDigit :: Parser Word8
pNonZeroDigit =
    (\c -> fromIntegral (ord c - 48))
        <$> MP.satisfy isNonZeroDigit
        <?> "Non-Zero Digit"

-- | Parses a single lower-case letter.
pLowerLetter :: Parser Char
pLowerLetter = MP.satisfy isLowerLetter <?> "Lower-case Letter"

-- | Parses a short string.
--
-- A short string is a string of length 1. It's equivalent to a Char in
-- Haskell, but it is delimited by double quotes in RES.
--
-- >>> MP.parseMaybe pShortString "\"Z\""
-- Just 'Z'
--
-- >>> MP.parseMaybe pShortString "\"\""
-- Nothing
--
-- >>> MP.parseMaybe pShortString "\"ab\""
-- Nothing
pShortString :: Parser Char
pShortString = (MP.single '"' *> pPrintable <* MP.single '"')
  where
    pPrintable :: Parser Char
    pPrintable =
        MP.satisfy isPrintable
            <|> (MP.chunk "\\\"" $> '"')
            <|> (MP.chunk "\\\\" $> '\\')

-- | Parses a string.
--
-- >>> MP.parseMaybe pString "\"\""
-- Just ""
--
-- >>> MP.parseMaybe pString "\"Hello World\""
-- Just "Hello World"
--
-- >>> MP.parseMaybe pString "\"Some escapes: \\\\ \\\".\""
-- Just "Some escapes: \\ \"."
--
-- >>> MP.parseMaybe pString "\""
-- Nothing
pString :: Parser Text
pString = (MP.single '"' *> pInString <* MP.single '"') <?> "String"
  where
    pInString :: Parser Text
    pInString = mconcat <$> MP.many (pPrintable <|> pEscaped)

    pPrintable :: Parser Text
    pPrintable = MP.takeWhile1P (Just "Printable Latin1 Character") isPrintable

    pEscaped :: Parser Text
    pEscaped =
        (MP.chunk "\\\"" $> Text.singleton '"')
            <|> (MP.chunk "\\\\" $> Text.singleton '\\')

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

-- | True if a character is whitespace.
isWhitespace :: Char -> Bool
isWhitespace c =
    (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') || (c == '\f')

-- | True if a character can appear in a mnemonic.
isMnemonicChar :: Char -> Bool
isMnemonicChar c =
    isUpperLetter c || isLowerLetter c || (c == '1') || (c == '0')

-- | True if a character is a lower-case letter.
--
-- >>> isLowerLetter 'a'
-- True
--
-- >>> isLowerLetter 'z'
-- True
--
-- >>> isLowerLetter 'Q'
-- False
isLowerLetter :: Char -> Bool
isLowerLetter = isCharInRange 'a' 'z'

-- | True if a character is an upper-case letter.
--
-- >>> isUpperLetter 'W'
-- True
--
-- >>> isUpperLetter 'w'
-- False
isUpperLetter :: Char -> Bool
isUpperLetter = isCharInRange 'A' 'Z'

-- | True if a character is allowed as part of a string.
--
-- This excludes un-escaped quotes and backslashes.
--
-- >>> isPrintable 'a'
-- True
--
-- >>> isPrintable '"'
-- False
--
-- >>> isPrintable '\\'
-- False
isPrintable :: Char -> Bool
isPrintable c = isLatin1 c && (c /= '"') && (c /= '\\')

-- | True if a character is part of the Latin1 / ISO-8859-1 code page of the
--   ASCII table.
--
-- NOTE: This also includes double-quotes and backslash.
--
-- >>> isLatin1 '~'
-- True
--
-- >>> isLatin1 '\t'
-- False
--
-- >>> isLatin1 '"'
-- True
isLatin1 :: Char -> Bool
isLatin1 c = (c' >= 0x0020 && c' <= 0x007E) || (c' >= 0x00A0 && c' <= 0x00FF)
  where
    c' :: Int
    c' = ord c

-- | Check if a character is within a supplied inclusive range.
isCharInRange
    :: Char   -- ^ Minimum of range (inclusive).
    -> Char   -- ^ Maximum of range (inclusive).
    -> Char   -- ^ Character to check.
    -> Bool   -- ^ True if the character is in the range.
isCharInRange rangeMin rangeMax c =
    (c' >= ord rangeMin) && (c' <= ord rangeMax)
  where
    c' :: Int
    c' = ord c

-- $setup
-- >>> :set -XOverloadedStrings
