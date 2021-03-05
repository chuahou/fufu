-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mahjong.Gen where

import Data.Array      (concat, concatMap, filter, length, replicate, nub, (..),
                        (!!))
import Data.Maybe      (Maybe (..), isJust)
import Effect          (Effect)
import Effect.Random   (randomInt)
import Prelude

import Mahjong.Hand

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

-- | Generates a random standard hand.
genStandard :: Effect Hand
genStandard = do
  tt <- genTatsu
  m1 <- genMentsu
  m2 <- genMentsu
  m3 <- genMentsu
  a  <- genTile
  let hand = Hand tt m1 m2 m3 a
  if isHand hand then pure hand else genStandard

-- | Generates a tanki hand.
genTanki :: Effect Hand
genTanki = do
  m1 <- genMentsu
  m2 <- genMentsu
  m3 <- genMentsu
  m4 <- genMentsu
  a  <- genTile
  let hand = Tanki m1 m2 m3 m4 a
  if isHand hand then pure hand else genTanki

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
    if nub xs /= xs then genChiitoi else pure $
      Chiitoi a b c d e f g

-- | Generates a hand, with lower probability of chiitoi.
genHand :: Effect Hand
genHand = join <<< genFromList <<< concat $
  [ replicate 1  genChiitoi
  , replicate 10 genStandard
  , replicate 2  genTanki
  ]
