module QuadRES.RES where

import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           Data.Word                      ( Word8
                                                , Word16
                                                )

-- | Identifier for a glyph.
data GlyphID
    = GlyphIDGardiner Text Word16 (Maybe Char)
    | GlyphIDMnemonic Text
    | GlyphIDOpen
    | GlyphIDClose
    | GlyphIDShortString Char
    deriving (Eq, Show)

-- | List of glyph arguments.
newtype GlyphArgs = GlyphArgs [GlyphArg] deriving (Eq, Show)

-- | Single glyph argument.
data GlyphArg
    = GlyphArgMirrorState MirrorState
    | GlyphArgRotate Rotate
    | GlyphArgScale Scale
    | GlyphArgScaleX ScaleX
    | GlyphArgScaleY ScaleY
    | GlyphArgColor Color
    | GlyphArgShadeState ShadeState
    | GlyphArgShadePattern ShadePattern
    deriving (Eq, Show)

-- | Rotation of a glyph.
newtype Rotate = Rotate Word16 deriving (Eq, Show)

-- | Uniform scaling of a glyph.
newtype Scale = Scale RealN deriving (Eq, Show)

-- | Scaling of a glyph in the x-direction.
newtype ScaleX = ScaleX RealN deriving (Eq, Show)

-- | Scaling of a glyph in the y-direction.
newtype ScaleY = ScaleY RealN deriving (Eq, Show)

-- | Multiple switches.
newtype Switches = Switches [Switch] deriving (Eq, Show)

-- | Switch list.
newtype Switch = Switch [SwitchArg] deriving (Eq, Show)

-- | Switch argument.
data SwitchArg
    = SwColor Color
    | SwShadeState ShadeState
    | SwSep Sep
    | SwFitState FitState
    | SwMirrorState MirrorState
    deriving (Eq, Show)

-- | Glyph separation.
newtype Sep = Sep RealN deriving (Eq, Show)

-- | Fit state.
data FitState
    = Fit
    | NoFit
    deriving (Eq, Show)

-- | Mirroring state.
data MirrorState
    = Mirror
    | NoMirror
    deriving (Eq, Show)

-- | Shade state.
data ShadeState
    = Shade
    | NoShade
    deriving (Eq, Show)

-- | A color.
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

-- | Area shading.
newtype ShadePattern = ShadePattern (NonEmpty Side)
    deriving (Eq, Show)

-- | Side to shade.
data Side
    = SideT
    | SideB
    | SideS
    | SideE
    deriving (Eq, Show)

-- | Real number, in the range 0.00 to 9.99.
--
-- Internally, the number is stored as a 'Word16' in hundredths precision
-- (ie. it has the range 0 to 999).
newtype RealN = RealN Word16 deriving (Eq, Ord)

instance Show RealN where
    show r = show ones <> "." <> show tenths <> show hundredths
        where (ones, tenths, hundredths) = realNDigits r

instance Num RealN where
    (RealN a) + (RealN b) =
        let c = a + b in if c <= 999 then RealN c else error "RealN overflow"
    (RealN a) - (RealN b)
        | b <= a    = RealN (b - a)
        | otherwise = error "RealN underfloat: there are no negative values"
    (*)    = error "multiplication is not yet implemented"
    negate = error
        "negate is not defined for RealN: there are no negative values"
    abs = id
    signum r@(RealN x) = if x == 0 then r else RealN 1
    fromInteger i = RealN (fromIntegral (i * 100))

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
