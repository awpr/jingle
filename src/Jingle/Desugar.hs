-- | Lowering of user-facing syntax to Core.
module Jingle.Desugar (dsTrackContents) where

import Control.Applicative ((<|>))

import Jingle.Core (Item(..), Phonon(..), Repeat(..), TrackContents)
import qualified Jingle.Syntax as S

dsItem
  :: Rational
  -> Maybe S.Articulation
  -> S.TrackPiece
  -> TrackContents Rational (Maybe S.Articulation) S.Chord
dsItem scale art (S.Single (S.Advance adv (S.Phonon d mx))) = case mx of
  Nothing -> []
  Just (S.Articulated x art') ->
    pure $ Single
      (if adv then scale * d else 0)
      (Phonon (scale * d) (art' <|> art) x)
dsItem scale art (S.Group cont scale' art') =
  dsTrackContents (scale * scale') (art' <|> art) cont
dsItem scale art (S.Rep (S.Repeat cont end n)) =
  pure $ Rep $ Repeat
    (dsTrackContents scale art cont)
    (dsTrackContents scale art end)
    n

dsTrackContents
  :: Rational
  -> Maybe S.Articulation
  -> S.TrackContents
  -> TrackContents Rational (Maybe S.Articulation) S.Chord
dsTrackContents scale art = concatMap (dsItem scale art)
