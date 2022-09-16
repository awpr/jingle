{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Jingle.ToMIDI (toMIDIFile, writeMIDIFile) where

import Data.Function ((&))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator)

import Control.Lens ((%~))
import Data.Text (Text)
import Data.Vector qualified as V

import Sound.MIDI.File qualified as MIDI
import Sound.MIDI.File.Save qualified as MIDI
import Sound.MIDI.File.Event qualified as Event
import Sound.MIDI.File.Event.Meta qualified as Meta
import Sound.MIDI.Message.Channel qualified as Channel
import Sound.MIDI.Message.Channel.Voice qualified as Voice
import Data.EventList.Relative.TimeBody qualified as TimeBody
import Data.EventList.Relative.TimeTime qualified as TimeTime
import Numeric.NonNegative.Class qualified as NN (C)
import Numeric.NonNegative.Wrapper qualified as NN

import Jingle.Core
import Jingle.Patches (toGeneralMIDIProgram)
import Jingle.Syntax
  ( ChordQuality(..), Chord(..), Interval(..)
  , Articulation(..), Accidental(..)
  )
import Jingle.Syntax qualified as Syntax
import Jingle.Types (Comp(..), Track(..), Note(..))

type FlatTrack t ann a = TimeTime.T t (Phonon t ann a)

flatten :: forall t ann a. NN.C t => TrackContents t ann a -> FlatTrack t ann a
flatten (Sequence el) = TimeTime.foldr (&) f TimeTime.pause el
 where
  f
    :: Item t (Phonon t ann a)
    -> FlatTrack t ann a
    -> t
    -> FlatTrack t ann a
  f (Single x) rest dt = TimeTime.cons dt x rest
  f (Rep (Repeat (Sequence content) (Sequence end) n)) rest dt =
    TimeTime.delay dt $ rep content end n rest

  rep
    :: TimeTime.T t (Item t (Phonon t ann a))
    -> TimeTime.T t (Item t (Phonon t ann a))
    -> Int
    -> FlatTrack t ann a
    -> FlatTrack t ann a
  rep _ _ n rest | n <= 0 = rest
  rep cont _ 1 rest =
    TimeTime.foldr (&) f (flip TimeTime.delay rest) cont
  rep cont end n rest =
    TimeTime.foldr (&) f
      (flip TimeTime.delay $ rep cont end (n-1) rest)
      (TimeTime.append cont end)

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

interpNote :: Syntax.Note -> Note
interpNote (Syntax.Named octave name acc) =
  Note $ 12 * fromMaybe 4 octave + (notes V.! fromEnum name) + shift
 where
  shift = case fromMaybe Natural acc of
    DoubleSharp -> 2
    Sharp -> 1
    Natural -> 0
    Flat -> -1
    DoubleFlat -> -2

  -- Starting from A, how many semitones is each note above the same-octave C?
  --
  -- The numbers are weird because the octave numbering system itself is weird.
  -- I don't make the rules.
  notes = V.fromList [9, 11, 0, 2, 4, 5, 7]

expandChord :: Chord -> [Note]
expandChord (Chord root q adds) =
  maybe pure expandChordQuality q root' ++
  map (\ (Interval x) -> Note x + root') adds
 where
  root' = interpNote root

noteToMIDI :: Note -> Voice.Pitch
noteToMIDI (Note x) = Voice.toPitch $ x + 12

quantizeTimes
  :: Integer
  -> FlatTrack NN.Rational ann a
  -> FlatTrack NN.Integer ann a
quantizeTimes denom =
  fmap (phDuration %~ truncate . (* fromIntegral denom)) .
  TimeTime.mapTime (truncate . (* fromIntegral denom))

interpArticulation :: Maybe Articulation -> (NN.Integer, Int)
interpArticulation art =
  case art of
    Nothing -> (7, 96)
    Just Staccato -> (4, 96)
    Just Marcato -> (4, 127)
    Just Accent -> (7, 127)
    Just Tenuto -> (8, 112)
    Just Legato -> (8, 96)

toMIDIEvents
  :: Channel.Channel
  -> Phonon NN.Integer (Maybe Articulation) Note
  -> [(NN.Integer, Event.T)]
toMIDIEvents chan (Phonon dur art x) =
  [ (0, mkEvent Voice.NoteOn)
  , ((durFactor * dur) `div` 8, mkEvent Voice.NoteOff)
  ]
 where
  mkEvent f =
    Event.MIDIEvent $
      Channel.Cons chan
        (Channel.Voice $ f (noteToMIDI x) (Voice.toVelocity vel))
  (durFactor, vel) = interpArticulation art

-- | Converte 'TimeTime.T' to 'TimeBody.T' with an optional terminator.
--
-- If 'Just', the result ends with the given value, after any trailing
-- delta-time of the 'TimeTime.T'; if 'Nothing', any trailing delta-time is
-- discarded and the result ends with the last @body@ in the 'TimeTime.T'.
toTimeBody :: Maybe a -> TimeTime.T t a -> TimeBody.T t a
toTimeBody end =
  TimeTime.foldr (&)
    (\ x rest dt -> TimeBody.cons dt x rest)
    (\ dt -> maybe id (TimeBody.cons dt) end TimeBody.empty)

programChange :: Channel.Channel -> Text -> Maybe Event.T
programChange chan =
  fmap
    (Event.MIDIEvent . Channel.Cons chan .
     Channel.Voice . Voice.ProgramChange) .
  toGeneralMIDIProgram

-- | Lower one 'FlatTrack' to a single channel of MIDI data.
toChannel
  :: Channel.Channel
  -> Integer
  -> Text
  -> FlatTrack NN.Rational (Maybe Articulation) Chord
  -> TimeTime.T Meta.ElapsedTime Event.T
toChannel chan denom voice =
  maybe id (TimeTime.cons 0) (programChange chan voice) .
  TimeTime.moveBackward .
  TimeTime.flatten .
  fmap (concatMap (toMIDIEvents chan) . phNote expandChord) .
  quantizeTimes denom

-- | All of the MIDI channel numbers usable for arbitrary instruments.
--
-- For whatever reason, General MIDI reserves channel 10 for percussion, so it
-- cannot be allocated to any non-percussion instrument.
normalChannels :: [Channel.Channel]
normalChannels = Channel.toChannel <$> [1..9] ++ [11..16]

toTrack
  :: Integer
  -> Maybe NN.Int
  -> [(Channel.Channel, Track (FlatTrack NN.Rational (Maybe Articulation) Chord))]
  -> MIDI.Track
toTrack denom tempo =
  maybe id (TimeBody.cons 0 . Event.MetaEvent . Meta.SetTempo) tempo .
  toTimeBody (Just $ Event.MetaEvent Meta.EndOfTrack) .
  foldr
    (\ (i, Track voice ch) tr -> TimeTime.merge (toChannel i denom voice ch) tr)
    (TimeTime.pause 0)

zipRest :: [a] -> [b] -> ([(a, b)], [b])
zipRest _ [] = ([], [])
zipRest [] ys = ([], ys)
zipRest (x:xs) (y:ys) = let !(xys, rest) = zipRest xs ys in ((x, y) : xys, rest)

zipChunks :: [a] -> [b] -> [[(a, b)]]
zipChunks _ [] = []
zipChunks xs ys =
  let !(xys, ys') = zipRest xs ys
  in  xys : zipChunks xs ys'

toMIDIFile :: Comp (TrackContents NN.Rational (Maybe Articulation) Chord) -> MIDI.T
toMIDIFile (Comp tempo ts) =
  MIDI.Cons
    (if length tracks > 1 then MIDI.Parallel else MIDI.Mixed)
    (MIDI.Ticks (NN.fromNumber $ fromIntegral denom))
    (zipWith (toTrack denom) (Just usPerQuarter : repeat Nothing) tracks)
 where
  flattened :: [Track (FlatTrack NN.Rational (Maybe Articulation) Chord)]
  flattened = fmap flatten <$> ts

  tracks :: [[(Channel.Channel, Track (FlatTrack NN.Rational (Maybe Articulation) Chord))]]
  tracks = zipChunks normalChannels flattened

  usPerQuarter = NN.fromNumber $ 60_000_000 `div` tempo
  denom = 8 * foldl' lcm 1
    [ denominator (NN.toNumber x)
    | t <- flattened
    , x <- TimeTime.getTimes (_trContents t)
    ]

writeMIDIFile
  :: FilePath
  -> Comp (TrackContents NN.Rational (Maybe Articulation) Chord)
  -> IO ()
writeMIDIFile p = MIDI.toFile p . toMIDIFile
