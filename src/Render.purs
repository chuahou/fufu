-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Render where

import Data.Array      (null)
import Data.Foldable   (foldl, traverse_)
import Data.Maybe      (Maybe (..))
import Effect          (Effect)
import Prelude
import Web.DOM.Node    (Node, appendChild, setTextContent)
import Web.DOM.Element (Element, setAttribute, setClassName, toNode)
import Web.HTML.HTMLImageElement as Img

import Mahjong.Hand (Tile (..))
import Mahjong.Scenario

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
  let elem' = Img.toElement elem
  setClassName "tile" elem'
  pure <<< toNode $ elem'

-- | Renders a scenario, given a function to query nodes by id with.
renderScenario :: (String -> Effect (Maybe Element)) -> Scenario -> Effect Unit
renderScenario q s = do
  -- Render hands

  -- Set winds
  q' "#bakaze" $ traverse_ (setWind s.bakaze) <<< Img.fromElement
  q' "#jikaze" $ traverse_ (setWind s.jikaze) <<< Img.fromElement

  -- Add doras
  q' "#dora" $ addTiles s.doras

  -- Add uradoras, hide otherwise
  q' "#ura" $ if null s.uras
              then setAttribute "style" "visibility:hidden"
              else addTiles s.uras

  -- Add riichi stick
  q' "#riibou" $ setAttribute "style" ("visibility:" <>
                  if s.riichi
                  then "visible"
                  else "hidden")

  -- Set tsumo/ron
  q' "#tsumoron" $ setTextContent (if s.tsumo then "TSUMO" else "RON") <<< toNode
  where
    -- Run query and perform effect on any returned element, doing nothing
    -- otherwise.
    q' :: String -> (Element -> Effect Unit) -> Effect Unit
    q' cs f = q cs >>= traverse_ f

    -- Sets src of img to black variant of wind w.
    setWind w img = Img.setSrc
      ("./riichi-mahjong-tiles/Black/" <> tileToFilename w) img

    -- Add tiles ts to div d.
    addTiles :: Array Tile -> Element -> Effect Unit
    addTiles ts d = foldl f (pure unit) ts
      where
        f n x = n >>= \_ -> createImage x >>= flip appendChild (toNode d)
