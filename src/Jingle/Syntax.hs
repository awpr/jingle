{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Jingle.Syntax
    ( Note(..), NoteName(..), Accidental(..)
    , ChordQuality(..)
    , Chord(..), cRoot, cQuality, cAdd
    , Interval(..)
    , NoteMeta(..), nmArticulation, nmDuration
    , Articulation(..)
    , Repeat(..), TrackPiece(..), TrackContents
    ) where

import GHC.Generics (Generic)

import Control.Lens.TH (makeLenses)

import Data.Portray (Portray, PortrayDataCons(..))
import Data.Wrapped (Wrapped(..))

data NoteName = A | B | C | D | E | F | G
  deriving (Generic, Eq, Ord, Enum, Show)
  deriving Portray via Wrapped Generic NoteName

data Accidental
  = Sharp | DoubleSharp | Natural | Flat | DoubleFlat
  deriving (Generic, Eq, Ord, Enum, Show)
  deriving Portray via Wrapped Generic Accidental

data Note
  = Named (Maybe Int) NoteName (Maybe Accidental)
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via Wrapped Generic Note

data ChordQuality
  = Fifth    -- 1    5
  | Maj Bool -- 1  3 5  [  7]
  | Min Bool -- 1 b3 5  [ b7]
  | Dom      -- 1  3 5    b7
  | Aug Bool -- 1  3 #5 [ b7]
  | Dim Bool -- 1 b3 b5 [bb7]
  | HalfDim  -- 1 b3 b5   b7
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via Wrapped Generic ChordQuality

newtype Interval = Interval { intervalValue :: Int }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via PortrayDataCons Interval

data Chord a = Chord
  { _cRoot :: a
  , _cQuality :: Maybe ChordQuality
  , _cAdd :: [Interval]
  }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via PortrayDataCons (Chord a)

$(makeLenses ''Chord)

data Articulation
  = Staccato
  | Marcato
  | Accent
  | Tenuto
  | Legato
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via Wrapped Generic Articulation

data NoteMeta = NoteMeta
  { _nmDuration :: Rational
  , _nmArticulation :: Maybe Articulation
  }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via Wrapped Generic NoteMeta

$(makeLenses ''NoteMeta)

data Repeat = Repeat
  { _repContents :: [TrackPiece]
  , _repEnding :: [TrackPiece]
  , _repCount :: Int
  }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via PortrayDataCons Repeat

data TrackPiece
  = Rest Rational
  | Play [Chord Note] NoteMeta
  | RepNote NoteMeta
  | Group [TrackPiece] NoteMeta
  | Par [TrackContents]
  | Rep Repeat
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via Wrapped Generic TrackPiece

type TrackContents = [TrackPiece]
