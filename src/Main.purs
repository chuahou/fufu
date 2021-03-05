module Main (main) where

import Prelude

import Data.Array            (init, last, sort)
import Data.Tuple            (Tuple (..))
import Data.Foldable         (foldl)
import Data.Maybe            (Maybe (..))
import Effect                (Effect)
import Web.DOM.Document      (toParentNode)
import Web.DOM.Element       (toNode)
import Web.DOM.Node          (appendChild)
import Web.DOM.ParentNode    (QuerySelector (..), querySelector)
import Web.HTML              (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window       (document)

import Mahjong.Gen
import Mahjong.Hand
import Render

main :: Effect Unit
main = do
  -- Generate a hand
  hand <- genHand

  -- Convert to array
  let handArr = sort $ handToList hand

  -- Display hand
  doc     <- toDocument <$> (window >>= document)
  handDiv <- querySelector (QuerySelector "#hand") (toParentNode doc)
  img     <- createImage (Manzu 1)
  case Tuple (handDiv) (init handArr) of
    Tuple (Just h) (Just xs) -> foldl (addImg h) (pure unit) xs
    _                        -> pure unit

  -- Display last tile
  agariDiv <- querySelector (QuerySelector "#agari") (toParentNode doc)
  case Tuple (agariDiv) (last handArr) of
    Tuple (Just a) (Just x) -> createImage x >>= flip appendChild (toNode a)
    _                       -> pure unit

  where
    addImg h e x = e >>= \_ -> createImage x >>= flip appendChild (toNode h)
