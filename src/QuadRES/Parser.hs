module QuadRES.Parser where

import           Control.Monad.Identity         ( Identity )
import           Data.Char                      ( ord )
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import qualified Text.Megaparsec               as MP

type Parsec e s a = MP.ParsecT e s Identity a

type Parser a = Parsec Void Text a

---- Auxiliary Definitions

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
