-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou
--
-- Representation of a winning mahjong hand. Does not include kokushi musou, as
-- the focus is on training scoring (scoring kokushi musou is easy but will be
-- bloat here).

module Mahjong.Hand ( Tile (..)
                    , succTile
                    , isTile
                    , Mentsu (..)
                    , isMentsu
                    , isShuntsu
                    , Hand (..)
                    , isHand
                    , Atama
                    ) where

import Data.Array    (concatMap, group, replicate, sort)
import Data.Foldable (all, length)
import Prelude

-- Tiles --

-- | Types of tiles.
data Tile = Manzu Int
          | Pinzu Int
          | Souzu Int
          | East
          | South
          | West
          | North
          | White
          | Green
          | Red

instance showTile :: Show Tile where
  show (Manzu n) = show n <> "m"
  show (Pinzu n) = show n <> "p"
  show (Souzu n) = show n <> "s"
  show East      = "1z"
  show South     = "2z"
  show West      = "3z"
  show North     = "4z"
  show White     = "5z"
  show Green     = "6z"
  show Red       = "7z"

instance eqTile :: Eq Tile where
  eq t1 t2 = eq (tileNo t1) (tileNo t2)

instance ordTile :: Ord Tile where
  compare t1 t2 = compare (tileNo t1) (tileNo t2)

-- | Convert Tile to a number for easy ordering/equality.
tileNo :: Tile -> Int
tileNo (Manzu n) = n
tileNo (Pinzu n) = 9 + n
tileNo (Souzu n) = 18 + n
tileNo East      = 28
tileNo South     = 29
tileNo West      = 30
tileNo North     = 31
tileNo White     = 32
tileNo Green     = 33
tileNo Red       = 34

-- | Successor function of tiles (e.g. for dora).
succTile :: Tile -> Tile
succTile (Manzu n) = Manzu (succTile' n)
succTile (Pinzu n) = Pinzu (succTile' n)
succTile (Souzu n) = Souzu (succTile' n)
succTile East      = South
succTile South     = West
succTile West      = North
succTile North     = East
succTile White     = Green
succTile Green     = Red
succTile Red       = White

-- | Successor function of tile number.
succTile' :: Int -> Int
succTile' 9 = 1
succTile' n = n + 1

-- | Checks if a tile is valid.
isTile :: Tile -> Boolean
isTile (Manzu n) = isTile' n
isTile (Pinzu n) = isTile' n
isTile (Souzu n) = isTile' n
isTile _         = true

-- | Checks if a number is valid.
isTile' :: Int -> Boolean
isTile' n = n >= 1 && n <= 9

-- Mentsu --

-- | A single mentsu.
data Mentsu = Shuntsu Tile Tile Tile -- ^ A run.
            | Kotsu   Tile           -- ^ A set.
            | Kantsu  Tile           -- ^ A quad.

-- | Checks if a mentsu is valid.
isMentsu :: Mentsu -> Boolean
isMentsu (Shuntsu (Manzu x) (Manzu y) (Manzu z)) = isShuntsu x y z
isMentsu (Shuntsu (Pinzu x) (Pinzu y) (Pinzu z)) = isShuntsu x y z
isMentsu (Shuntsu (Souzu x) (Souzu y) (Souzu z)) = isShuntsu x y z
isMentsu (Kotsu t)                               = isTile t
isMentsu (Kantsu t)                              = isTile t
isMentsu _                                       = false

-- | Checks if a shuntsu is valid.
isShuntsu :: Int -> Int -> Int -> Boolean
isShuntsu x y z = check <<< sort $ [x, y, z]
    where
        check xs@[a, b, c] = (b == a + 1) && (c == b + 1) && all isTile' xs
        check _            = false

-- Hand --

-- | Winning mahjong hand.
data Hand = Hand Mentsu Mentsu Mentsu Mentsu Atama     -- ^ Standard hand.
          | Chiitoi Tile Tile Tile Tile Tile Tile Tile -- ^ Seven pairs.

instance showHand :: Show Hand where
  show = show <<< sort <<< handToList

-- | Checks if a hand is valid.
isHand :: Hand -> Boolean
isHand h = checkCounts (handToList h) &&
  case h of
    Hand m1 m2 m3 m4 t    -> all isMentsu [m1, m2, m3, m4] && isTile t
    Chiitoi a b c d e f g -> all isTile [a, b, c, d, e, f, g]

-- | Gets tiles occuring in a hand as a list.
handToList :: Hand -> Array Tile
handToList (Hand m1 m2 m3 m4 t) = concatMap mentsuToList [m1, m2, m3, m4] <> [t, t]
  where
    mentsuToList (Shuntsu x y z) = [x, y, z]
    mentsuToList (Kotsu x)       = replicate 3 x
    mentsuToList (Kantsu x)      = replicate 4 x
handToList (Chiitoi a b c d e f g) = concatMap (replicate 2) [a, b, c, d, e, f, g]

-- | Check that no tile in the given list occurs more than four times.
checkCounts :: Array Tile -> Boolean
checkCounts = all ((_ <= 4) <<< length) <<< group <<< sort

type Atama = Tile
