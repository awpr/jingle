{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ToMIDI (toMIDIFile, writeMIDIFile) where

import Control.Applicative ((<|>))
import Data.List (foldl', sortOn)
import Data.Ratio (denominator)

import Control.Lens ((%~), _2)
import Control.Lens.TH (makeLenses)

import qualified Sound.MIDI.File as MIDI
import qualified Sound.MIDI.File.Save as MIDI
import qualified Sound.MIDI.File.Event as Event
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Wrapper as NN

import AST

flatten :: [TrackPiece] -> [Musel Rational (Articulated Voicing)]
flatten ts = foldr (go 1 Nothing) [] ts
 where
  adjust
    :: Rational
    -> Maybe Articulation
    -> Musel Rational (Articulated Voicing)
    -> Musel Rational (Articulated Voicing)
  adjust d a (Musel d' ph) = Musel (d * d') $ case ph of
    Nothing -> Nothing
    Just (Phonon (Articulated x a') adv) ->
       Just $ Phonon (Articulated x (a' <|> a)) adv

  go
    :: Rational
    -> Maybe Articulation
    -> TrackPiece
    -> [Musel Rational (Articulated Voicing)]
    -> [Musel Rational (Articulated Voicing)]
  go d a (Single x) rest = adjust d a x : rest
  go d a (Group ps d' a') rest = foldr (go (d * d') (a' <|> a)) rest ps
  go d a (Rep (Repeat content end n)) rest = rep d a content end n rest

  rep
    :: Rational
    -> Maybe Articulation
    -> [TrackPiece]
    -> [TrackPiece]
    -> Int
    -> [Musel Rational (Articulated Voicing)]
    -> [Musel Rational (Articulated Voicing)]
  rep _ _ _ _ n rest | n <= 0 = rest
  rep d a cont _ 1 rest = foldr (go d a) rest cont
  rep d a cont end n rest = foldr (go d a) (rep d a cont end (n-1) rest) (cont ++ end)

-- Random wishlist items:
-- * Default to the octave closest to the last note
-- * Repeat the last chord
-- * Distinguish "simultaneous" notes from "grouped" notes:
--     C,E,G/4 as a chord held for a 16th note, vs.
--     C&E&G/4 having C,E for a quarter and G for a 16th

expandChordQuality :: ChordQuality -> Note -> [Note]
expandChordQuality q root = root : case q of
  Fifth -> [fifth, octave]
  Maj sv -> [third, fifth] ++ [seventh | sv]
  Min sv -> [third - 1, fifth] ++ [seventh - 1 | sv]
  Dom -> [third, fifth, seventh - 1]
  Aug sv -> [third, fifth + 1] ++ [seventh - 1 | sv]
  Dim sv -> [third - 1, fifth - 1] ++ [seventh - 2 | sv]
  HalfDim -> [third - 1, fifth - 1, seventh - 1]
 where
  third = root + 4
  fifth = root + 7
  seventh = root + 11
  octave = root + 12

expandChord :: Voicing -> [Note]
expandChord (Voicing root (Chord q adds)) =
  maybe pure expandChordQuality q root ++
  map (\ (Interval x) -> Note x + root) adds

noteToMIDI :: Note -> Voice.Pitch
noteToMIDI (Note x) = Voice.toPitch $ x + 12

quantizeTimes :: Int -> [Musel Rational a] -> [Musel Int a]
quantizeTimes denom = map (muDuration %~ truncate . (* fromIntegral denom))

data Timestamped a = Timestamped { _tsTime :: Integer, _tsVal :: a }
  deriving Functor

$(makeLenses ''Timestamped)

_unused :: ()
_unused = const () (tsTime @() @[])

toTimestamped :: [Musel Int a] -> [Timestamped (Int, a)]
toTimestamped = go 0
 where
  go _ [] = []
  go t (Musel d c : ms) =
    maybe
      id
      (\ (Phonon v _) -> (Timestamped t (d, v) :))
      c
      (go (t + if maybe True _phAdvance c then fromIntegral d else 0) ms)

interpArticulation :: Maybe Articulation -> (Int, Int)
interpArticulation art =
  case art of
    Nothing -> (7, 64)
    Just Staccato -> (4, 64)
    Just Marcato -> (4, 96)
    Just Accent -> (7, 96)
    Just Tenuto -> (8, 80)
    Just Legato -> (8, 64)

data MIDINote = MIDINote Voice.Pitch Voice.Velocity

toMIDINote :: (Int, Articulated Note) -> (Int, MIDINote)
toMIDINote (dur, Articulated x art) =
  let (durFactor, vel) = interpArticulation art
  in  ((durFactor * dur) `div` 8, MIDINote (noteToMIDI x) (Voice.toVelocity vel))

deltaEncode :: [Timestamped a] -> [(NN.Integer, a)]
deltaEncode = go 0
 where
  go _ [] = []
  go t (Timestamped t' x : xs) = (NN.fromNumber (t' - t), x) : go t' xs

toEvents :: [Timestamped (Int, MIDINote)] -> [Timestamped Event.T]
toEvents =
  concatMap
    (\ (Timestamped t (d, x)) ->
      [ Timestamped t (wrap Voice.NoteOn x)
      , Timestamped (t + fromIntegral d) (wrap Voice.NoteOff x)
      ])

 where
  wrap f (MIDINote x vel) = Event.MIDIEvent $
    Channel.Cons (Channel.toChannel 0) (Channel.Voice $ f x vel)

toTrack :: Int -> [TrackPiece] -> MIDI.Track
toTrack denom pcs =
  EventList.fromPairList $ deltaEncode $ sortOn _tsTime $ toEvents $
  map (fmap toMIDINote) $
  concatMap (tsVal . _2 . arVal $ expandChord) $
  toTimestamped $
  quantizeTimes denom $
  flatten pcs

toMIDIFile :: Comp -> MIDI.T
toMIDIFile (Comp _tempo ts) =
  MIDI.Cons
    (if length ts > 1 then MIDI.Parallel else MIDI.Mixed)
    (MIDI.Ticks (NN.fromNumber $ fromIntegral denom))
    (map (toTrack (fromIntegral denom) . _trContents) ts)
 where
  denom = 8 * foldl' lcm 1 [denominator x | t <- ts, p <- _trContents t, x <- durations p ]

writeMIDIFile :: FilePath -> Comp -> IO ()
writeMIDIFile p = MIDI.toFile p . toMIDIFile
