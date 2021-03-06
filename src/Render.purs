-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Render where

import Data.Array      (concatMap, filter, replicate, null, sort, (:))
import Data.Foldable   (foldl, traverse_)
import Data.Maybe      (Maybe (..))
import Data.Tuple      (Tuple (..), fst, snd)
import Effect          (Effect)
import Prelude
import Web.DOM.Node    (Node, appendChild, setTextContent)
import Web.DOM.Element (Element, setAttribute, setClassName, toNode)
import Web.HTML.HTMLImageElement as Img

import Mahjong.Gen (genFromList)
import Mahjong.Hand
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


-- | Creates image element from given filename and class.
createImage :: String -> String -> Effect Node
createImage fname c = do
  elem <- Img.create
  Img.setSrc fname elem
  let elem' = Img.toElement elem
  setClassName c elem'
  pure <<< toNode $ elem'

-- | Creates image element for a tile.
createTileImage :: Tile -> Effect Node
createTileImage t = do
  let fname = "./riichi-mahjong-tiles/Regular/" <> tileToFilename t
  createImage fname "tile"

-- | Creates image element for closed tile (e.g. for closed kan).
createClosedImage :: Effect Node
createClosedImage = do
  let fname = "./riichi-mahjong-tiles/Regular/Back.svg"
  createImage fname "tile closed"

-- | Renders a scenario, given a function to query nodes by id with.
renderScenario :: (String -> Effect (Maybe Element)) -> Scenario -> Effect Unit
renderScenario q s = do
  -- Render closed tiles
  let isRenderedOpen (Tuple _ (Kantsu _)) = true
      isRenderedOpen m                    = fst m
  let tehai = case s.hand of
                Hand tt m1 m2 m3 a -> tatsuToArray tt
                                   <> replicate 2 a
                                   <> concatMap (mentsuToArray <<< snd)
                                        (filter (not <<< isRenderedOpen) [m1, m2, m3])
                Tanki m1 m2 m3 m4 a -> a : concatMap (mentsuToArray <<< snd)
                                        (filter (not <<< isRenderedOpen) [m1, m2, m3])
                Chiitoi a b c d e f g -> g : concatMap (replicate 2)
                                        [a, b, c, d, e, f]
  q' "#hand" $ addTiles <<< sort $ tehai

  -- Render open mentsu
  let naki = case s.hand of
                Hand    _ m1 m2 m3 _  -> filter isRenderedOpen [m1, m2, m3]
                Tanki   m1 m2 m3 m4 _ -> filter isRenderedOpen [m1, m2, m3, m4]
                Chiitoi _ _ _ _ _ _ _ -> []
  q' "#naki" $ addNaki naki

  -- Render agari
  q' "#agari" $ addTiles [s.agari]

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
        f n x = n >>= \_ -> createTileImage x >>= flip appendChild (toNode d)

    -- Add called mentsu ms to div d.
    addNaki :: Array HMentsu -> Element -> Effect Unit
    addNaki ms d = foldl f (pure unit) ms
      where
        f n x = n >>= \_ -> case x of
                      Tuple true  m@(Kantsu _) -> addTiles (mentsuToArray m) d
                      Tuple false m@(Kantsu _) -> renderClosedKantsu m
                      Tuple _     m            -> addTiles (mentsuToArray m) d
        renderClosedKantsu (Kantsu t) = do
          createClosedImage >>= flip appendChild (toNode d)
          addTiles [t, t] d
          createClosedImage >>= flip appendChild (toNode d)
        renderClosedKantsu _ = pure unit
