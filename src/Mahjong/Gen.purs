-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mahjong.Gen where

import Data.Array      (concat, filter, length, replicate, nub, (..), (!!), (:))
import Data.Maybe      (Maybe (..))
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
genShuntsu = mkShuntsu <$> genFromList startTiles
    where
        startTiles = filter f allTiles

        -- Filter out 8s, 9s and word tiles.
        f (Manzu n) = n <= 7
        f (Pinzu n) = n <= 7
        f (Souzu n) = n <= 7
        f _         = false

        -- Make a shuntsu from a start tile.
        mkShuntsu t = Shuntsu t (succTile t) (succTile $ succTile t)

-- | Generates a random kotsu.
genKotsu :: Effect Mentsu
genKotsu = Kotsu <$> genTile

-- | Generates a random kantsu.
genKantsu :: Effect Mentsu
genKantsu = Kantsu <$> genTile

-- | Generates a random mentsu.
-- The number of occurrences in the list is the relative probability of each
-- occurring.
genMentsu :: Effect Mentsu
genMentsu = join <<< genFromList $ concat
    [ [ genKantsu ]
    , replicate 5 genKotsu
    , replicate 10 genShuntsu
    ]

-- | Generates a random standard hand.
genStandard :: Effect Hand
genStandard = do
    m1 <- genMentsu
    m2 <- genMentsu
    m3 <- genMentsu
    m4 <- genMentsu
    a  <- genTile
    let hand = Hand m1 m2 m3 m4 a
    if isHand hand then pure hand else genStandard

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
genHand = join <<< genFromList $ genChiitoi : replicate 10 genStandard
