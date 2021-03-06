module Main (main) where

import Prelude

import Data.Array            (concatMap, head, replicate, sort, (:))
import Data.Tuple            (Tuple (..), snd)
import Data.Foldable         (foldl, foldr, null)
import Data.Maybe            (Maybe (..))
import Effect                (Effect)
import Effect.Class.Console  (error)
import Web.DOM.Document      (toParentNode)
import Web.DOM.Element       (setAttribute, toNode)
import Web.DOM.Node          (appendChild)
import Web.DOM.ParentNode    (QuerySelector (..), querySelector)
import Web.HTML              (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window       (document)

import Mahjong.Gen
import Mahjong.Hand
import Mahjong.Scenario
import Render

main :: Effect Unit
main = do
  -- Get document node
  doc <- toParentNode <<< toDocument <$> (window >>= document)
  let query q = querySelector (QuerySelector q) doc

  renderScenario query ({ hand:   Hand
                                    (Ryanmen $ Manzu 2)
                                    (closed $ Shuntsu $ Manzu 2)
                                    (closed $ Shuntsu $ Manzu 3)
                                    (open   $ Shuntsu $ Manzu 4)
                                    (Manzu 9)
                        , bakaze: South
                        , jikaze: West
                        , doras:  [ North, White ]
                        , uras:   []
                        , riichi: false
                        , tsumo:  false
                        })
