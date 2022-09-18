{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Lowering of user-facing syntax to Core.
module Jingle.Desugar (dsScore, dsTrackContents, lowerNotes) where

import Control.Applicative ((<|>))
import Control.Monad.State (State, evalState, state)

import Control.Lens (over, traverseOf)
import Data.EventList.Relative.TimeTime qualified as TimeTime
import Numeric.NonNegative.Wrapper qualified as NN

import Jingle.Core
         ( Item(..), Phonon(..), Repeat(..), Note(..)
         , TrackContents, Sequence(..)
         , phNote
         )
import qualified Jingle.Syntax as S
import Jingle.Syntax (NoteName(..), Accidental(..))
import Jingle.Types (Score(..), trContents, scTracks)

dsItem
  :: Rational
  -> Maybe S.Articulation
  -> S.TrackPiece
  -> TrackContents NN.Rational (Maybe S.Articulation) (S.Chord S.Note)
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
  -> TrackContents NN.Rational (Maybe S.Articulation) (S.Chord S.Note)
dsTrackContents scale art = foldMap (dsItem scale art)

dsScore
  :: Score S.TrackContents
  -> Score (TrackContents NN.Rational (Maybe S.Articulation) (S.Chord S.Note))
dsScore = fmap (dsTrackContents 1 Nothing)

pitchClass :: S.NoteName -> Maybe Accidental -> Int
pitchClass nm acc = nm' + acc'
 where
  nm' = case nm of
    C -> 0
    D -> 2
    E -> 4
    F -> 5
    G -> 7
    A -> 9
    B -> 11
  acc' = case acc of
    Nothing -> 0
    Just DoubleFlat -> -2
    Just Flat -> -1
    Just Natural -> 0
    Just Sharp -> 1
    Just DoubleSharp -> 2

lowerNote
  :: S.Note -> State (Maybe Note) Note
lowerNote (S.Named oct nm acc) = state $ \case
  Nothing -> (Note (cl + 48), Just $ Note (cl + 48))
  Just (Note prev) ->
    let pitch = case oct of
          Nothing -> (cl - prev + 6) `mod` 12 + prev - 6
          Just o -> cl + o * 12
    in  (Note pitch, Just $ Note pitch)
 where
  cl = pitchClass nm acc

lowerNotes
  :: Score (TrackContents NN.Rational (Maybe S.Articulation) (S.Chord S.Note))
  -> Score (TrackContents NN.Rational (Maybe S.Articulation) (S.Chord Note))
lowerNotes =
  over (scTracks . traverse . trContents) $
  flip evalState Nothing .
  traverseOf (traverse . phNote . S.cRoot) lowerNote
