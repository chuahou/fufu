-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Render where

import Effect       (Effect)
import Prelude
import Web.DOM.Node (Node)
import Web.HTML.HTMLImageElement as Img

import Mahjong.Hand (Tile (..))

-- | Generates filename for a tile.
tileToFilename :: Tile -> String
tileToFilename t = go t <> ".svg"
  where
    go (Manzu n) = "Man" <> show n
    go (Pinzu n) = "Pin" <> show n
    go (Souzu n) = "Sou" <> show n
    go East      = "Ton"
    go South     = "Nan"
    go West      = "Shaa"
    go North     = "Pei"
    go White     = "Haku"
    go Green     = "Hatsu"
    go Red       = "Chun"

-- | Creates image element for a tile.
createImage :: Tile -> Effect Node
createImage t = do
  let fname = "./riichi-mahjong-tiles/Regular/" <> tileToFilename t
  elem <- Img.create
  Img.setSrc fname elem
  pure <<< Img.toNode $ elem
