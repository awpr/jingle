{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Lowering of user-facing syntax to Core.
module Jingle.Desugar (dsScore, dsTrackContents, lowerNotes) where

import Control.Applicative ((<|>))
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State (State, evalState, get, put, state)
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

type DsM = ReaderT NoteMeta (State [S.Chord S.Note])

dsArticulation :: Maybe Articulation -> DsM (Maybe Articulation)
dsArticulation x = (x <|>) <$> asks _nmArticulation

dsDuration :: Rational -> DsM NN.Rational
dsDuration d = NN.fromNumber . (d *) <$> asks _nmDuration

dsNote
  :: NoteMeta
  -> [S.Chord S.Note]
  -> DsM (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])
dsNote (NoteMeta d art) x = do
  art' <- dsArticulation art
  d' <- dsDuration d
  return $ Sequence $
      TimeTime.cons 0
        (Single $ Phonon d' art' x)
        (TimeTime.pause d')

withMeta :: NoteMeta -> DsM a -> DsM a
withMeta (NoteMeta d art) =
  local (\ (NoteMeta d' art') -> NoteMeta (d * d') (art <|> art'))

dsItem
  :: S.TrackPiece
  -> DsM (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])

dsItem (S.Rest d) = Sequence . TimeTime.pause <$> dsDuration d

dsItem (S.Play x meta) = put x >> dsNote meta x

dsItem (S.RepNote meta) = get >>= dsNote meta

dsItem (S.Group cont meta) = withMeta meta $ dsTrackContents cont

dsItem (S.Par trs) = do
  trs' <- traverse (fmap getItems . dsTrackContents) trs
  return $ Sequence $ foldr TimeTime.merge (TimeTime.pause 0) trs'

dsItem (S.Rep (S.Repeat cont end n)) = do
  rep <- Repeat <$> dsTrackContents cont <*> dsTrackContents end <*> pure n
  return $ Sequence $ TimeTime.cons 0 (Rep rep) (TimeTime.pause 0)

dsTrackContents
  :: S.TrackContents
  -> DsM (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])
dsTrackContents = getAp . foldMap (Ap . dsItem)

dsScore
  :: Score S.TrackContents
  -> Score (TrackContents NN.Rational (Maybe S.Articulation) [S.Chord S.Note])
dsScore = fmap
  (flip evalState [] . flip runReaderT (NoteMeta 1 Nothing) . dsTrackContents)

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
