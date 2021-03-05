module Main (main) where

import Prelude

import Data.Array            (concatMap, head, replicate, sort, (:))
import Data.Tuple            (Tuple (..))
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
import Render

-- | @RenderHand ts ns a@ is a hand to be rendered with drawn tiles @ts@, called
-- | tiles @ns@ and winning tile @a@.
data RenderHand = RenderHand (Array Tile) (Array Tile) Tile

toRenderHand :: Hand -> Maybe RenderHand
toRenderHand h = case h of
  Hand tt m1 m2 m3 a    -> goHand tt a $ splitKans [m1, m2, m3]
  Tanki m1 m2 m3 m4 a   -> goTanki a   $ splitKans [m1, m2, m3, m4]
  Chiitoi a b c d e f g -> Just $ RenderHand
    ((concatMap (replicate 2) [a, b, c, d, e, f]) <> [g]) [] g
  where
    splitKans = foldr go (Tuple [] [])
    go m@(Kantsu t) (Tuple ts ns) = Tuple ts (ns <> mentsuToArray m)
    go m            (Tuple ts ns) = Tuple (ts <> mentsuToArray m) ns

    goHand tt a (Tuple ts ns) = case join $ head <$> checkTatsu tt of
      Just agari -> Just $ RenderHand (a:a:tatsuToArray tt <> ts) ns agari
      _          -> Nothing
    goTanki a (Tuple ts ns) = Just $ RenderHand (a:ts) ns a

main :: Effect Unit
main = do
  -- Generate a hand
  maybeRenderHand <- toRenderHand <$> genHand
  RenderHand hand naki agari <- case maybeRenderHand of
                                     Just rh -> pure rh
                                     Nothing -> error "Invalid hand generated"
                                             >>= \_ -> pure $ RenderHand [] [] East

  -- Get document node
  doc <- toParentNode <<< toDocument <$> (window >>= document)
  let query q = querySelector (QuerySelector q) doc

  -- Display hand
  query "#hand" >>= \x -> case x of
    Just h -> foldl (addImg h) (pure unit) <<< sort $ hand
    _      -> pure unit

  -- Display called tiles
  query "#naki" >>= \x -> case x of
      Just n -> if null naki
                  then setAttribute "style" "display:none" n
                  else foldl (addImg n) (pure unit) <<< sort $ naki
      _      -> pure unit

  -- Display last tile
  query "#agari" >>= \x -> case x of
    Just a -> createImage agari >>= flip appendChild (toNode a)
    _      -> pure unit

  where
    addImg d e x = e >>= \_ -> createImage x >>= flip appendChild (toNode d)
