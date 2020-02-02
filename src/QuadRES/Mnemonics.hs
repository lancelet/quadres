{-|
Module      : QuadRES.Mnemonics
Description : Corrected Manuel de Codage mnemonics.

This list was taken from here:
  https://mjn.host.cs.st-andrews.ac.uk/egyptian/standards/mnemonics.txt
-}
{-# LANGUAGE OverloadedStrings #-}
module QuadRES.Mnemonics where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )

import           QuadRES.Gardiner               ( Gardiner
                                                , gardiner
                                                )
import qualified QuadRES.Gardiner              as G

mnemonics :: Map Text Gardiner
mnemonics = Map.fromList
    [ ("mSa"  , gardiner G.A 12)
    , ("xr"   , gardiner G.A 15)
    , ("Xrd"  , gardiner G.A 17)
    , ("sr"   , gardiner G.A 21)
    , ("mniw" , gardiner G.A 33)
    , ("qiz"  , gardiner G.A 38)
    , ("iry"  , gardiner G.A 47)
    , ("Sps"  , gardiner G.A 50)
    , ("Spsi" , gardiner G.A 51)
    , ("msi"  , gardiner G.B 3)
    , ("DHwty", gardiner G.C 3)
    , ("Xnmw" , gardiner G.C 4)
    , ("inpw" , gardiner G.C 6)
    , ("stX"  , gardiner G.C 7)
    , ("mnw"  , gardiner G.C 8)
    , ("mAat" , gardiner G.C 10)
    , ("HH"   , gardiner G.C 11)
    , ("tp"   , gardiner G.D 1)
    , ("Hr"   , gardiner G.D 2)
    , ("Sny"  , gardiner G.D 3)
    , ("ir"   , gardiner G.D 4)
    , ("rmi"  , gardiner G.D 9)
    , ("wDAt" , gardiner G.D 10)
    , ("fnD"  , gardiner G.D 19)
    , ("r"    , gardiner G.D 21)
    , ("rA"   , gardiner G.D 21)
    , ("spt"  , gardiner G.D 24)
    , ("spty" , gardiner G.D 25)
    , ("mnD"  , gardiner G.D 27)
    , ("kA"   , gardiner G.D 28)
    , ("aHA"  , gardiner G.D 34)
    , ("a"    , gardiner G.D 36)
    , ("Dsr"  , gardiner G.D 45)
    , ("d"    , gardiner G.D 46)
    , ("Dba"  , gardiner G.D 50)
    , ("mt"   , gardiner G.D 52)
    , ("rd"   , gardiner G.D 56)
    , ("sbq"  , gardiner G.D 56)
    , ("gH"   , gardiner G.D 56)
    , ("gHs"  , gardiner G.D 56)
    , ("b"    , gardiner G.D 58)
    , ("ab"   , gardiner G.D 59)
    , ("wab"  , gardiner G.D 60)
    , ("sAH"  , gardiner G.D 61)
    , ("zzmt" , gardiner G.E 6)
    , ("zAb"  , gardiner G.E 17)
    , ("mAi"  , gardiner G.E 22)
    , ("l"    , gardiner G.E 23)
    , ("rw"   , gardiner G.E 23)
    , ("Aby"  , gardiner G.E 24)
    , ("wn"   , gardiner G.E 34)
    , ("HAt"  , gardiner G.F 4)
    , ("SsA"  , gardiner G.F 5)
    , ("wsr"  , gardiner G.F 12)
    , ("wp"   , gardiner G.F 13)
    , ("db"   , gardiner G.F 16)
    , ("Hw"   , gardiner G.F 18)
    , ("bH"   , gardiner G.F 18)
    , ("ns"   , gardiner G.F 20)
    , ("idn"  , gardiner G.F 21)
    , ("msDr" , gardiner G.F 21)
    , ("sDm"  , gardiner G.F 21)
    , ("DrD"  , gardiner G.F 21)
    , ("pH"   , gardiner G.F 22)
    , ("kfA"  , gardiner G.F 22)
    , ("xpS"  , gardiner G.F 23)
    , ("wHm"  , gardiner G.F 25)
    , ("Xn"   , gardiner G.F 26)
    , ("sti"  , gardiner G.F 29)
    , ("Sd"   , gardiner G.F 30)
    , ("ms"   , gardiner G.F 31)
    , ("X"    , gardiner G.F 32)
    , ("sd"   , gardiner G.F 33)
    , ("ib"   , gardiner G.F 34)
    , ("nfr"  , gardiner G.F 35)
    , ("zmA"  , gardiner G.F 36)
    , ("imAx" , gardiner G.F 39)
    , ("Aw"   , gardiner G.F 40)
    , ("spr"  , gardiner G.F 42)
    , ("iwa"  , gardiner G.F 44)
    , ("isw"  , gardiner G.F 44)
    , ("pXr"  , gardiner G.F 46)
    , ("qAb"  , gardiner G.F 46)
    , ("A"    , gardiner G.G 1)
    , ("AA"   , gardiner G.G 2)
    , ("tyw"  , gardiner G.G 4)
    , ("mwt"  , gardiner G.G 14)
    , ("nbty" , gardiner G.G 16)
    , ("m"    , gardiner G.G 17)
    , ("mm"   , gardiner G.G 18)
    , ("nH"   , gardiner G.G 21)
    , ("Db"   , gardiner G.G 22)
    , ("rxyt" , gardiner G.G 23)
    , ("Ax"   , gardiner G.G 25)
    , ("dSr"  , gardiner G.G 27)
    , ("gm"   , gardiner G.G 28)
    , ("bA"   , gardiner G.G 29)
    , ("baHi" , gardiner G.G 32)
    , ("aq"   , gardiner G.G 35)
    , ("wr"   , gardiner G.G 36)
    , ("gb"   , gardiner G.G 38)
    , ("zA"   , gardiner G.G 39)
    , ("pA"   , gardiner G.G 40)
    , ("xn"   , gardiner G.G 41)
    , ("wSA"  , gardiner G.G 42)
    , ("w"    , gardiner G.G 43)
    , ("ww"   , gardiner G.G 44)
    , ("mAw"  , gardiner G.G 46)
    , ("TA"   , gardiner G.G 47)
    , ("snD"  , gardiner G.G 54)
    , ("wSm"  , gardiner G.H 2)
    , ("pq"   , gardiner G.H 2)
    , ("pAq"  , gardiner G.H 3)
    , ("nr"   , gardiner G.H 4)
    , ("Sw"   , gardiner G.H 6)
    , ("aSA"  , gardiner G.I 1)
    , ("Styw" , gardiner G.I 2)
    , ("mzH"  , gardiner G.I 3)
    , ("sbk"  , gardiner G.I 4)
    , ("sAq"  , gardiner G.I 5)
    , ("km"   , gardiner G.I 6)
    , ("Hfn"  , gardiner G.I 8)
    , ("f"    , gardiner G.I 9)
    , ("D"    , gardiner G.I 10)
    , ("DD"   , gardiner G.I 11)
    , ("in"   , gardiner G.K 1)
    , ("ad"   , gardiner G.K 3)
    , ("XA"   , gardiner G.K 4)
    , ("bz"   , gardiner G.K 5)
    , ("nSmt" , gardiner G.K 6)
    , ("xpr"  , gardiner G.L 1)
    , ("bit"  , gardiner G.L 2)
    , ("srqt" , gardiner G.L 7)
    , ("iAm"  , gardiner G.M 1)
    , ("Hn"   , gardiner G.M 2)
    , ("xt"   , gardiner G.M 3)
    , ("rnp"  , gardiner G.M 4)
    , ("tr"   , gardiner G.M 6)
    , ("SA"   , gardiner G.M 8)
    , ("zSn"  , gardiner G.M 9)
    , ("wdn"  , gardiner G.M 11)
    , ("xA"   , gardiner G.M 12)
    , ("wAD"  , gardiner G.M 13)
    , ("HA"   , gardiner G.M 16)
    , ("i"    , gardiner G.M 17)
    , ("ii"   , gardiner G.M 18)
    , ("sxt"  , gardiner G.M 20)
    , ("sm"   , gardiner G.M 21)
    , ("sw"   , gardiner G.M 23)
    , ("rsw"  , gardiner G.M 24)
    , ("Sma"  , gardiner G.M 26)
    , ("nDm"  , gardiner G.M 29)
    , ("bnr"  , gardiner G.M 30)
    , ("bdt"  , gardiner G.M 34)
    , ("Dr"   , gardiner G.M 36)
    , ("iz"   , gardiner G.M 40)
    , ("pt"   , gardiner G.N 1)
    , ("iAdt" , gardiner G.N 4)
    , ("idt"  , gardiner G.N 4)
    , ("ra"   , gardiner G.N 5)
    , ("zw"   , gardiner G.N 5)
    , ("hrw"  , gardiner G.N 5)
    , ("Hnmmt", gardiner G.N 8)
    , ("pzD"  , gardiner G.N 9)
    , ("Abd"  , gardiner G.N 11)
    , ("iaH"  , gardiner G.N 11)
    , ("sbA"  , gardiner G.N 14)
    , ("dwA"  , gardiner G.N 14)
    , ("dwAt" , gardiner G.N 15)
    , ("tA"   , gardiner G.N 16)
    , ("iw"   , gardiner G.N 18)
    , ("wDb"  , gardiner G.N 20)
    , ("spAt" , gardiner G.N 24)
    , ("xAst" , gardiner G.N 25)
    , ("Dw"   , gardiner G.N 26)
    , ("Axt"  , gardiner G.N 27)
    , ("xa"   , gardiner G.N 28)
    , ("q"    , gardiner G.N 29)
    , ("iAt"  , gardiner G.N 30)
    , ("n"    , gardiner G.N 35)
    , ("mw"   , G.Gardiner G.N 35 G.Va)
    , ("S"    , gardiner G.N 37)
    , ("Sm"   , gardiner G.N 40)
    , ("id"   , gardiner G.N 41)
    , ("pr"   , gardiner G.O 1)
    , ("h"    , gardiner G.O 4)
    , ("Hwt"  , gardiner G.O 6)
    , ("aH"   , gardiner G.O 11)
    , ("wsxt" , gardiner G.O 15)
    , ("kAr"  , gardiner G.O 18)
    , ("zH"   , gardiner G.O 22)
    , ("txn"  , gardiner G.O 25)
    , ("iwn"  , gardiner G.O 28)
    , ("aA"   , gardiner G.O 29)
    , ("zxnt" , gardiner G.O 30)
    , ("z"    , gardiner G.O 34)
    , ("zb"   , gardiner G.O 35)
    , ("inb"  , gardiner G.O 36)
    , ("Szp"  , gardiner G.O 42)
    , ("ipt"  , gardiner G.O 45)
    , ("nxn"  , gardiner G.O 47)
    , ("niwt" , gardiner G.O 49)
    , ("zp"   , gardiner G.O 50)
    , ("Snwt" , gardiner G.O 51)
    , ("wHa"  , gardiner G.P 4)
    , ("nfw"  , gardiner G.P 5)
    , ("TAw"  , gardiner G.P 5)
    , ("aHa"  , gardiner G.P 6)
    , ("xrw"  , gardiner G.P 8)
    , ("st"   , gardiner G.Q 1)
    , ("wz"   , gardiner G.Q 2)
    , ("p"    , gardiner G.Q 3)
    , ("qrsw" , gardiner G.Q 6)
    , ("xAwt" , gardiner G.R 1)
    , ("xAt"  , gardiner G.R 1)
    , ("Htp"  , gardiner G.R 4)
    , ("kAp"  , gardiner G.R 5)
    , ("kp"   , gardiner G.R 5)
    , ("snTr" , gardiner G.R 7)
    , ("nTr"  , gardiner G.R 8)
    , ("bd"   , gardiner G.R 9)
    , ("dd"   , gardiner G.R 11)
    , ("Dd"   , gardiner G.R 11)
    , ("imnt" , gardiner G.R 14)
    , ("iAb"  , gardiner G.R 15)
    , ("wx"   , gardiner G.R 16)
    , ("xm"   , gardiner G.R 22)
    , ("HDt"  , gardiner G.S 1)
    , ("dSrt" , gardiner G.S 3)
    , ("N"    , gardiner G.S 3)
    , ("sxmty", gardiner G.S 6)
    , ("xprS" , gardiner G.S 7)
    , ("Atf"  , gardiner G.S 8)
    , ("Swty" , gardiner G.S 9)
    , ("mDH"  , gardiner G.S 10)
    , ("wsx"  , gardiner G.S 11)
    , ("nbw"  , gardiner G.S 12)
    , ("tHn"  , gardiner G.S 15)
    , ("THn"  , gardiner G.S 15)
    , ("mnit" , gardiner G.S 18)
    , ("sDAw" , gardiner G.S 19)
    , ("xtm"  , gardiner G.S 20)
    , ("sT"   , gardiner G.S 22)
    , ("dmD"  , gardiner G.S 23)
    , ("Tz"   , gardiner G.S 24)
    , ("Sndyt", gardiner G.S 26)
    , ("mnxt" , gardiner G.S 27)
    , ("s"    , gardiner G.S 29)
    , ("sf"   , gardiner G.S 30)
    , ("siA"  , gardiner G.S 32)
    , ("Tb"   , gardiner G.S 33)
    , ("anx"  , gardiner G.S 34)
    , ("Swt"  , gardiner G.S 35)
    , ("xw"   , gardiner G.S 37)
    , ("HqA"  , gardiner G.S 38)
    , ("awt"  , gardiner G.S 39)
    , ("wAs"  , gardiner G.S 40)
    , ("Dam"  , gardiner G.S 41)
    , ("abA"  , gardiner G.S 42)
    , ("xrp"  , gardiner G.S 42)
    , ("sxm"  , gardiner G.S 42)
    , ("md"   , gardiner G.S 43)
    , ("Ams"  , gardiner G.S 44)
    , ("nxxw" , gardiner G.S 45)
    , ("HD"   , gardiner G.T 3)
    , ("HDD"  , gardiner G.T 6)
    , ("pd"   , gardiner G.T 9)
    , ("pD"   , gardiner G.T 10)
    , ("zin"  , gardiner G.T 11)
    , ("zwn"  , gardiner G.T 11)
    , ("sXr"  , gardiner G.T 11)
    , ("Ai"   , gardiner G.T 12)
    , ("Ar"   , gardiner G.T 12)
    , ("rwd"  , gardiner G.T 12)
    , ("rwD"  , gardiner G.T 12)
    , ("rs"   , gardiner G.T 13)
    , ("qmA"  , gardiner G.T 14)
    , ("wrrt" , gardiner G.T 17)
    , ("Sms"  , gardiner G.T 18)
    , ("qs"   , gardiner G.T 19)
    , ("sn"   , gardiner G.T 22)
    , ("iH"   , gardiner G.T 24)
    , ("DbA"  , gardiner G.T 25)
    , ("Xr"   , gardiner G.T 28)
    , ("nmt"  , gardiner G.T 29)
    , ("sSm"  , gardiner G.T 31)
    , ("nm"   , gardiner G.T 34)
    , ("mA"   , gardiner G.U 1)
    , ("mr"   , gardiner G.U 6)
    , ("it"   , gardiner G.U 10)
    , ("HqAt" , gardiner G.U 11)
    , ("hb"   , gardiner G.U 13)
    , ("Sna"  , gardiner G.U 13)
    , ("tm"   , gardiner G.U 15)
    , ("biA"  , gardiner G.U 16)
    , ("grg"  , gardiner G.U 17)
    , ("stp"  , gardiner G.U 21)
    , ("mnx"  , gardiner G.U 22)
    , ("Ab"   , gardiner G.U 23)
    , ("Hmt"  , gardiner G.U 24)
    , ("wbA"  , gardiner G.U 26)
    , ("DA"   , gardiner G.U 28)
    , ("rtH"  , gardiner G.U 31)
    , ("zmn"  , gardiner G.U 32)
    , ("ti"   , gardiner G.U 33)
    , ("xsf"  , gardiner G.U 34)
    , ("Hm"   , gardiner G.U 36)
    , ("mxAt" , gardiner G.U 38)
    , ("100"  , gardiner G.V 1)
    , ("sTA"  , gardiner G.V 2)
    , ("sTAw" , gardiner G.V 3)
    , ("wA"   , gardiner G.V 4)
    , ("snT"  , gardiner G.V 5)
    , ("Ss"   , gardiner G.V 6)
    , ("Sn"   , gardiner G.V 7)
    , ("arq"  , gardiner G.V 12)
    , ("T"    , gardiner G.V 13)
    , ("iTi"  , gardiner G.V 15)
    , ("mDt"  , gardiner G.V 19)
    , ("XAr"  , gardiner G.V 19)
    , ("TmA"  , gardiner G.V 19)
    , ("10"   , gardiner G.V 20)
    , ("mD"   , gardiner G.V 20)
    , ("mH"   , gardiner G.V 22)
    , ("wD"   , gardiner G.V 24)
    , ("aD"   , gardiner G.V 26)
    , ("H"    , gardiner G.V 28)
    , ("wAH"  , gardiner G.V 29)
    , ("sk"   , gardiner G.V 29)
    , ("nb"   , gardiner G.V 30)
    , ("k"    , gardiner G.V 31)
    , ("msn"  , gardiner G.V 32)
    , ("sSr"  , gardiner G.V 33)
    , ("idr"  , gardiner G.V 37)
    , ("bAs"  , gardiner G.W 2)
    , ("Hb"   , gardiner G.W 3)
    , ("Xnm"  , gardiner G.W 9)
    , ("iab"  , gardiner G.W 10)
    , ("nzt"  , gardiner G.W 11)
    , ("g"    , gardiner G.W 11)
    , ("Hz"   , gardiner G.W 14)
    , ("xnt"  , gardiner G.W 17)
    , ("mi"   , gardiner G.W 19)
    , ("Hnqt" , gardiner G.W 22)
    , ("nw"   , gardiner G.W 24)
    , ("ini"  , gardiner G.W 25)
    , ("t"    , gardiner G.X 1)
    , ("rdi"  , gardiner G.X 8)
    , ("di"   , gardiner G.X 8)
    , ("mDAt" , gardiner G.Y 1)
    , ("mnhd" , gardiner G.Y 3)
    , ("zS"   , gardiner G.Y 3)
    , ("mn"   , gardiner G.Y 5)
    , ("ibA"  , gardiner G.Y 6)
    , ("zSSt" , gardiner G.Y 8)
    , ("y"    , gardiner G.Z 4)
    , ("W"    , gardiner G.Z 7)
    , ("imi"  , gardiner G.Z 11)
    , ("x"    , gardiner G.Aa 1)
    , ("Hp"   , gardiner G.Aa 5)
    , ("qn"   , gardiner G.Aa 8)
    , ("mAa"  , gardiner G.Aa 11)
    , ("im"   , gardiner G.Aa 13)
    , ("gs"   , gardiner G.Aa 13)
    , ("M"    , gardiner G.Aa 13)
    , ("sA"   , gardiner G.Aa 17)
    , ("apr"  , gardiner G.Aa 20)
    , ("wDa"  , gardiner G.Aa 21)
    , ("nD"   , gardiner G.Aa 27)
    , ("qd"   , gardiner G.Aa 28)
    , ("Xkr"  , gardiner G.Aa 30)
    ]