module Main (main) where

import Prelude

import Effect         (Effect)
import Effect.Console (log)

import Mahjong.Gen
import Mahjong.Hand

main :: Effect Unit
main = do
  hand <- genHand
  log <<< show $ hand
