module QuadRES.RES where

import           Data.Word                      ( Word8
                                                , Word16
                                                )

data Color
    = Black
    | Red
    | Green
    | Blue
    | White
    | Aqua
    | Fuchsia
    | Gray
    | Lime
    | Maroon
    | Navy
    | Olive
    | Purple
    | Silver
    | Teal
    | Yellow
    deriving (Eq, Show)

-- | Real number, in the range 0.00 to 9.99.
--
-- Internally, the number is stored as a 'Word16' in hundredths precision
-- (ie. it has the range 0 to 999).
newtype RealN = RealN Word16 deriving Eq

instance Show RealN where
    show r = show ones <> "." <> show tenths <> show hundredths
        where (ones, tenths, hundredths) = realNDigits r

-- | Creates an instance of a 'RealN' number, clamping digits as appropriate.
--
-- >>> mkRealN 0 5 2
-- 0.52
--
-- >>> mkRealN 5 2 5
-- 5.25
mkRealN
    :: Word8  -- ^ Ones digit (0-9).
    -> Word8  -- ^ Tenths digit (0-9).
    -> Word8  -- ^ Hundredths digit (0-9).
    -> RealN
mkRealN a b c = RealN (100 * f a + 10 * f b + f c)
  where
    f :: Word8 -> Word16
    f = fromIntegral . clamp 0 9

-- | Get the digits of a RealN number.
--
-- The digits are the ones, tenths and hundredths in a tuple. This is
-- effectively the inverse of 'mkReal'.
--
-- >>> realNDigits (mkRealN 0 4 2)
-- (0,4,2)
realNDigits :: RealN -> (Word8, Word8, Word8)
realNDigits (RealN w) = (ones, tenths, hundredths)
  where
    ones, tenths, hundredths :: Word8
    hundredths = f (w `mod` 10)
    tenths     = f ((w `div` 10) `mod` 10)
    ones       = f ((w `div` 100) `mod` 10)

    f :: Word16 -> Word8
    f = fromIntegral

-- | Clamp a value between two inclusive bounds.
--
-- >>> clamp 1.0 2.0 0.5
-- 1.0
--
-- >>> clamp 1.0 2.0 2.5
-- 2.0
--
-- >>> clamp 1.0 2.0 1.5
-- 1.5
clamp
    :: (Ord a)
    => a  -- ^ Minimum allowed value (inclusive).
    -> a  -- ^ Maximum allowed value (inclusive).
    -> a  -- ^ Input value.
    -> a  -- ^ Value clamped to lie between minimum and maximum inclusive.
clamp minAllowed maxAllowed x | x < minAllowed = minAllowed
                              | x > maxAllowed = maxAllowed
                              | otherwise      = x
