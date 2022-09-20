{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Lowering of user-facing syntax to Core.
module Jingle.Desugar (dsScore, dsTrackContents, lowerNotes) where

import Control.Applicative ((<|>))
import Control.Monad.Reader (Reader, asks, local, runReader)
import Control.Monad.State (State, evalState, put, state)
import Data.Monoid (Ap(..))

import Control.Lens (over, traverseOf, (^?))
import Data.EventList.Relative.TimeTime qualified as TimeTime
import Numeric.NonNegative.Wrapper qualified as NN

import Jingle.Core
         ( Item(..), Phonon(..), Repeat(..), Note(..)
         , TrackContents, Sequence(..)
         , phNote
         )
import qualified Jingle.Syntax as S
import Jingle.Syntax (NoteMeta(..), NoteName(..), Accidental(..), Articulation)
import Jingle.Types (Score(..), trContents, scTracks)

dsArticulation :: Maybe Articulation -> Reader NoteMeta (Maybe Articulation)
dsArticulation x = (x <|>) <$> asks _nmArticulation

dsDuration :: Rational -> Reader NoteMeta NN.Rational
dsDuration d = NN.fromNumber . (d *) <$> asks _nmDuration

withMeta :: NoteMeta -> Reader NoteMeta a -> Reader NoteMeta a
withMeta (NoteMeta d art) =
  local (\ (NoteMeta d' art') -> NoteMeta (d * d') (art <|> art'))

dsItem
  :: S.TrackPiece
  -> Reader NoteMeta
       (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])

dsItem (S.Rest d) = Sequence . TimeTime.pause <$> dsDuration d

dsItem (S.Play x (NoteMeta d art)) = do
  art' <- dsArticulation art
  d' <- dsDuration d
  return $ Sequence $
      TimeTime.cons 0
        (Single $ Phonon d' art' x)
        (TimeTime.pause d')

dsItem (S.Group cont meta) = withMeta meta $ dsTrackContents cont

dsItem (S.Par trs) = do
  trs' <- traverse (fmap getItems . dsTrackContents) trs
  return $ Sequence $ foldr TimeTime.merge (TimeTime.pause 0) trs'

dsItem (S.Rep (S.Repeat cont end n)) = do
  rep <- Repeat <$> dsTrackContents cont <*> dsTrackContents end <*> pure n
  return $ Sequence $ TimeTime.cons 0 (Rep rep) (TimeTime.pause 0)

dsTrackContents
  :: S.TrackContents
  -> Reader NoteMeta
       (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])
dsTrackContents = getAp . foldMap (Ap . dsItem)

dsScore
  :: Score S.TrackContents
  -> Score (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])
dsScore = fmap (flip runReader (NoteMeta 1 Nothing) . dsTrackContents)

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

-- | Returns the element of [lb, lb+n) congruent to x mod n.
--
-- In particular, @modInto 0 n x = x `mod` n@.
modInto :: Integral a => a -> a -> a -> a
modInto lb n x = (x - lb) `mod` n + lb

lowerNote
  :: S.Note -> State (Maybe Note) Note
lowerNote (S.Named oct nm acc) = state $ \mprev ->
  let cl = pitchClass nm acc
      pitch = case oct of
        Nothing ->
          maybe
            (cl + 48)
            (\ (Note prev) -> modInto (prev - 6) 12 cl)
            mprev
        Just o -> cl + o * 12
  in  (Note pitch, Just $ Note pitch)

lowerCluster
  :: [S.Chord S.Note] -> State (Maybe Note) [S.Chord Note]
lowerCluster xs = do
  r <- traverseOf (traverse . S.cRoot) lowerNote xs
  put (r ^? traverse . S.cRoot)
  return r

lowerNotes
  :: Score (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])
  -> Score (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord Note])
lowerNotes =
  over (scTracks . traverse . trContents) $
  flip evalState Nothing .
  traverseOf (traverse . phNote) lowerCluster
