module Main (main) where

import Prelude

import Effect                (Effect)
import Web.DOM.Document      (toParentNode)
import Web.DOM.ParentNode    (QuerySelector (..), querySelector)
import Web.HTML              (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window       (document)

import Mahjong.Hand
import Render

main :: Effect Unit
main = do
  -- Get document node
  doc <- toParentNode <<< toDocument <$> (window >>= document)
  let query q = querySelector (QuerySelector q) doc

  renderScenario query ({ hand:   Hand
                                    (Ryanmen $ Manzu 2)
                                    (closed $ Shuntsu $ Manzu 2)
                                    (closed $ Kantsu  $ Manzu 8)
                                    (open   $ Shuntsu $ Manzu 4)
                                    (Manzu 9)
                        , agari:  Manzu 1
                        , bakaze: South
                        , jikaze: West
                        , doras:  [ North, White ]
                        , uras:   []
                        , riichi: false
                        , tsumo:  false
                        })
