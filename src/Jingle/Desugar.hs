{-# LANGUAGE ImportQualifiedPost #-}

-- | Lowering of user-facing syntax to Core.
module Jingle.Desugar (dsTrackContents) where

import Control.Applicative ((<|>))

import Data.EventList.Relative.TimeTime qualified as TimeTime
import Numeric.NonNegative.Wrapper qualified as NN

import Jingle.Core (Item(..), Phonon(..), Repeat(..), TrackContents, Sequence(..))
import qualified Jingle.Syntax as S

dsItem
  :: Rational
  -> Maybe S.Articulation
  -> S.TrackPiece
  -> TrackContents NN.Rational (Maybe S.Articulation) S.Chord
dsItem scale art (S.Single (S.Advance adv (S.Phonon d mx))) = Sequence $
  case mx of
    Nothing -> TimeTime.pause dt
    Just (S.Articulated x art') ->
      TimeTime.cons 0
        (Single $ Phonon (NN.fromNumber $ scale * d) (art' <|> art) x)
        (TimeTime.pause dt)
 where
  dt = if adv then NN.fromNumber (scale * d) else 0
dsItem scale art (S.Group cont scale' art') =
  dsTrackContents (scale * scale') (art' <|> art) cont
dsItem scale art (S.Rep (S.Repeat cont end n)) =
  Sequence $
    TimeTime.cons 0
      (Rep $ Repeat
        (dsTrackContents scale art cont)
        (dsTrackContents scale art end)
        n)
      (TimeTime.pause 0)

dsTrackContents
  :: Rational
  -> Maybe S.Articulation
  -> S.TrackContents
  -> TrackContents NN.Rational (Maybe S.Articulation) S.Chord
dsTrackContents scale art = foldMap (dsItem scale art)
