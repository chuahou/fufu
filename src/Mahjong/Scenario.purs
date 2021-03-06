-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mahjong.Scenario where

import Data.Array    (filter, length)
import Data.Foldable (and, elem)
import Data.Maybe    (Maybe (..))
import Data.Tuple    (Tuple (..))
import Prelude

import Mahjong.Hand

-- | A complete scenario to be scored.
type Scenario =
  { hand   :: Hand
  , agari  :: Tile
  , bakaze :: Tile
  , jikaze :: Tile
  , doras  :: Array Tile
  , uras   :: Array Tile
  , riichi :: Boolean
  , tsumo  :: Boolean
  }

-- | Checks if scenario is valid.
isScenario :: Scenario -> Boolean
isScenario s = and [ checkHand
                   , checkAgari
                   , checkTiles
                   , checkBakaze
                   , checkJikaze
                   , checkDoraNumber
                   , checkUrasRiichi
                   ]
  where
    checkHand       = isHand s.hand
    checkAgari      = case s.hand of
                           Hand tt _ _ _ _ ->
                              case checkTatsu tt of
                                   Just xs -> s.agari `elem` xs
                                   Nothing -> false
                           Tanki _ _ _ _ a -> s.agari == a
                           Chiitoi _ _ _ _ _ _ g -> s.agari == g
    checkTiles      = checkCounts $ handToArray s.hand
                                 <> [ s.agari
                                    , s.bakaze
                                    , s.jikaze
                                    ]
                                 <> s.doras <> s.uras
    checkBakaze     = s.bakaze `elem` [East, South]
    checkJikaze     = s.jikaze `elem` [East, South, West, North]
    checkDoraNumber = let doras = length s.doras
                          uras  = length s.uras
                       in and [ doras >= 1
                              , doras <= 5
                              , doras >= 1 + countKans
                              , uras == 0 || uras == doras
                              ]
    countKans = case s.hand of
      Hand  _ m1 m2 m3 _    -> length <<< filter isKan $ [m1, m2, m3]
      Tanki m1 m2 m3 m4 _   -> length <<< filter isKan $ [m1, m2, m3, m4]
      Chiitoi _ _ _ _ _ _ _ -> 0
      where
        isKan (Tuple _ (Kantsu _)) = true
        isKan _                    = false
    checkUrasRiichi = s.riichi /= (length s.uras == 0)
