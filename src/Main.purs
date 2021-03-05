module Main (main) where

import Prelude

import Effect         (Effect)
import Effect.Console (log)

import Mahjong.Hand (Tile(..), succTile)

main :: Effect Unit
main = do
  log <<< show <<< succTile $ Manzu 3
