{-|
Module      : QuadRES.Gardiner
Description : Gardiner signs.
-}
module QuadRES.Gardiner where

import           Data.Word                      ( Word16 )

gardiner :: Group -> Word16 -> Gardiner
gardiner group number = Gardiner group number VNone

data Gardiner = Gardiner Group Word16 Variant

data Variant
    = VNone
    | Va
    | Vb
    | Vc

data Group
    = A
    | Aa
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | K
    | L
    | M
    | N
    | NL
    | NU
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
