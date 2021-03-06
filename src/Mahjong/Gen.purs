-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mahjong.Gen where

import Data.Array      (concat, concatMap, filter, length, replicate, nub, (..),
                        (!!))
import Data.Maybe      (Maybe (..), isJust)
import Effect          (Effect)
import Effect.Random   (random, randomInt)
import Prelude

import Mahjong.Hand

-- Basic Generators --

-- | One of each possible tile.
allTiles :: Array Tile
allTiles = concat [ map Manzu $ 1..9
                  , map Pinzu $ 1..9
                  , map Souzu $ 1..9
                  , [East, South, West, North, White, Green, Red]
                  ]

-- | Possible number tiles from @low@ to @high@ (inclusive).
rangeTiles :: Int -> Int -> Array Tile
rangeTiles a b = filter p allTiles
  where p t = isJust $ maybeNumber (\_ n -> n >= a && n <= b) t

-- | Randomly picks an element from the given array
genFromList :: forall a. Array a -> Effect a
genFromList xs = randomInt 0 (length xs - 1)
               >>= \i -> case (xs !! i) of
                   Just x  -> pure x
                   Nothing -> genFromList xs

-- | Generates a random tile.
genTile :: Effect Tile
genTile = genFromList allTiles

-- Mentsu Generators --

-- | Generates a random shuntsu.
genShuntsu :: Effect Mentsu
genShuntsu = Shuntsu <$> genFromList (rangeTiles 1 7)

-- | Generates a random kotsu.
genKotsu :: Effect Mentsu
genKotsu = Kotsu <$> genTile

-- | Generates a random kantsu.
genKantsu :: Effect Mentsu
genKantsu = Kantsu <$> genTile

-- | Generates a random mentsu.
-- | The number of occurrences in the list is the relative probability of each
-- | occurring.
genMentsu :: Effect Mentsu
genMentsu = join <<< genFromList $ concat
    [ [ genKantsu ]
    , replicate 5 genKotsu
    , replicate 10 genShuntsu
    ]

-- Tatsu Generators --

-- | Generates a random ryanmen tatsu.
genRyanmen :: Effect Tatsu
genRyanmen = Ryanmen <$> genFromList (rangeTiles 2 7)

-- | Generates a random penchan tatsu.
genPenchan :: Effect Tatsu
genPenchan = Penchan <$> genFromList (concatMap f [Manzu, Pinzu, Souzu])
  where f suit = [suit 1, suit 8]

-- | Generates a random kanchan tatsu.
genKanchan :: Effect Tatsu
genKanchan = Kanchan <$> genFromList (rangeTiles 1 7)

-- | Generates a random shanpon tatsu.
genShanpon :: Effect Tatsu
genShanpon = Shanpon <$> genTile

-- | Generates a random tatsu with probability proportional to parameter to
-- | @replicate@.
genTatsu :: Effect Tatsu
genTatsu = join <<< genFromList <<< concat $
  [ replicate 6 genRyanmen
  , replicate 3 genKanchan
  , replicate 2 genPenchan
  , replicate 2 genShanpon
  ]

-- Hand Generators --

-- | Generates a random riichi hand.
genRiichi :: Effect Hand
genRiichi = do
  m1 <- closed <$> genMentsu
  m2 <- closed <$> genMentsu
  m3 <- closed <$> genMentsu
  a  <- genTile
  random >>= \x -> if (x < 0.8)
    then do
      tt <- genTatsu
      let hand = Hand tt m1 m2 m3 a
      if isHand hand then pure hand else genRiichi
    else  do
      m4 <- closed <$> genMentsu
      let hand = Tanki m1 m2 m3 m4 a
      if isHand hand then pure hand else genRiichi

-- | Generates a chiitoi hand.
genChiitoi :: Effect Hand
genChiitoi = do
  a <- genTile
  b <- genTile
  c <- genTile
  d <- genTile
  e <- genTile
  f <- genTile
  g <- genTile
  let xs = [a, b, c, d, e, f, g]
  riichi <- (_ < 0.8) <$> random
  if nub xs /= xs then genChiitoi else pure $ Chiitoi a b c d e f g

-- | Generates a hand, with lower probability of chiitoi.
genHand :: Effect Hand
genHand = join <<< genFromList <<< concat $
  [ replicate 1  genChiitoi
  , replicate 10 genRiichi
  ]
