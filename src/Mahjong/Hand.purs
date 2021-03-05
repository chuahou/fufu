-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou
--
-- Representation of a winning mahjong hand. Does not include kokushi musou, as
-- the focus is on training scoring (scoring kokushi musou is easy but will be
-- bloat here).

module Mahjong.Hand where

import Data.Array    (concat, concatMap, group, replicate, sort)
import Data.Foldable (all, length)
import Data.Maybe    (Maybe (..), isJust)
import Data.Tuple    (Tuple (..), snd)
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

-- | @maybeNumber f t@ runs @f@ on the suit constructor and number of @t@ if @t@
-- | is a number tile, returning @Just@ of the result, and returning @Nothing@
-- | for word tiles.
maybeNumber :: forall a. ((Int -> Tile) -> Int -> a) -> Tile -> Maybe a
maybeNumber f (Manzu n) = Just $ f Manzu n
maybeNumber f (Pinzu n) = Just $ f Pinzu n
maybeNumber f (Souzu n) = Just $ f Souzu n
maybeNumber _ _         = Nothing

-- | @transformNumber f t@ runs @f@ on number arguments of number tiles, doing
-- | nothing for word tiles.
transformNumber :: (Int -> Int) -> Tile -> Tile
transformNumber f t = case maybeNumber f' t of
  Just y  -> y
  Nothing -> t
  where
    f' suit n = suit $ f n

-- Mentsu --

-- | A single complete mentsu.
data Mentsu = Shuntsu Tile -- ^ A run where the lowest tile is the parameter.
            | Kotsu   Tile -- ^ A set.
            | Kantsu  Tile -- ^ A quad.

-- | Checks if a mentsu is valid.
isMentsu :: Mentsu -> Boolean
isMentsu (Shuntsu (Manzu x)) = isShuntsu x
isMentsu (Shuntsu (Pinzu x)) = isShuntsu x
isMentsu (Shuntsu (Souzu x)) = isShuntsu x
isMentsu (Kotsu t)           = isTile t
isMentsu (Kantsu t)          = isTile t
isMentsu _                   = false

-- | Checks if a shuntsu is valid.
isShuntsu :: Int -> Boolean
isShuntsu x = x >= 1 && x <= 7

-- | Returns an array of tiles in a mentsu. Assumes mentsu is valid.
mentsuToArray :: Mentsu -> Array Tile
mentsuToArray (Shuntsu t) = [t, succTile t, succTile <<< succTile $ t]
mentsuToArray (Kotsu t)   = replicate 3 t
mentsuToArray (Kantsu t)  = replicate 4 t

-- Tatsu --

-- | A tatsu to be completed into mentsu with the winning tile.
data Tatsu = Ryanmen Tile -- ^ Two-sided wait for shuntsu, parameter is smaller tile.
           | Penchan Tile -- ^ Penchan wait for shuntsu, parameter is 1 or 8.
           | Kanchan Tile -- ^ Closed wait for shuntsu, parameter is smaller tile.
           | Shanpon Tile -- ^ Winning side of shanpon wait.

-- | Checks if a tatsu is valid, returning @Just ws@ where @ws@ are the winning
-- | tiles if so, and @Nothing@ otherwise.
checkTatsu :: Tatsu -> Maybe (Array Tile)
checkTatsu (Ryanmen t) = join $ maybeNumber go t
  where
    go suit n | n >= 2 && n <= 7 = Just <<< map suit $ [n - 1, n + 2]
              | otherwise        = Nothing
checkTatsu (Penchan t) = join $ maybeNumber go t
  where
    go suit n | n == 1    = Just [suit 3]
              | n == 8    = Just [suit 7]
              | otherwise = Nothing
checkTatsu (Kanchan t) = join $ maybeNumber go t
  where
    go suit n | n >= 1 && n <= 7 = Just [suit $ n + 1]
              | otherwise        = Nothing
checkTatsu (Shanpon t) = Just [t]

-- | Returns an array of tiles in a tatsu. Assumes tatsu is valid.
tatsuToArray :: Tatsu -> Array Tile
tatsuToArray (Ryanmen t) = [t, succTile t]
tatsuToArray (Penchan t) = [t, succTile t]
tatsuToArray (Kanchan t) = [t, succTile <<< succTile $ t]
tatsuToArray (Shanpon t) = [t, t]

-- Hand --

-- | A tuple of a mentsu and whether it's open, standing for "Hand Mentsu".
type HMentsu = Tuple Boolean Mentsu

-- | Create a closed mentsu.
closed :: Mentsu -> HMentsu
closed = Tuple false

-- | Create an open mentsu.
open :: Mentsu -> HMentsu
open = Tuple true

-- | Winning mahjong hand.
data Hand = Hand Tatsu HMentsu HMentsu HMentsu Atama    -- ^ Standard hand.
          | Tanki HMentsu HMentsu HMentsu HMentsu Atama -- ^ Standard w/ tanki.
          | Chiitoi Tile Tile Tile Tile Tile Tile Tile  -- ^ Seven pairs.

-- | A hand with a boolean indicating if there was riichi.
type RiichiHand = Tuple Boolean Hand

instance showHand :: Show Hand where
  show h = (case h of
                Hand  _ _ _ _ _       -> "Hand "
                Tanki _ _ _ _ _       -> "Tanki "
                Chiitoi _ _ _ _ _ _ _ -> "Chiitoi ")
         <> (show <<< sort <<< handToArray $ h)

-- | Checks if a hand is valid.
isHand :: Hand -> Boolean
isHand h = checkCounts (handToArray h) &&
  case h of
    Hand    tt m1 m2 m3 t -> all (isMentsu <<< snd) [m1, m2, m3] && isTile t && isJust (checkTatsu tt)
    Tanki   m1 m2 m3 m4 t -> all (isMentsu <<< snd) [m1, m2, m3, m4] && isTile t
    Chiitoi a b c d e f g -> all isTile [a, b, c, d, e, f, g]

-- | Gets tiles occuring in a hand as a array.
handToArray :: Hand -> Array Tile
handToArray (Hand tt m1 m2 m3 a) = concat
  [ concatMap (mentsuToArray <<< snd) [m1, m2, m3]
  , [a, a]
  , tatsuToArray tt
  , case checkTatsu tt of -- winning tiles
         Just agari -> agari
         Nothing    -> []
  ]
handToArray (Tanki m1 m2 m3 m4 a)   = concatMap (mentsuToArray <<< snd) [m1, m2, m3, m4] <> [a, a]
handToArray (Chiitoi a b c d e f g) = concatMap (replicate 2) [a, b, c, d, e, f, g]

-- | Check that no tile in the given list occurs more than four times.
checkCounts :: Array Tile -> Boolean
checkCounts = all ((_ <= 4) <<< length) <<< group <<< sort

type Atama = Tile
