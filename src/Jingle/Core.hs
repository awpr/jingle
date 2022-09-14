{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- | A normalized, ascetic representation of synthesizer-bound music.
--
-- This differs from the higher-level abstract syntax in that it dispenses with
-- all the syntactic sugar, notational shorthands, and compromises to
-- user-facing convenience, in favor of simplicity and flexibility.
--
-- In order to give the user-facing syntax and parser more flexibility, we
-- tolerate some duplication between the Core and AST datatypes.

module Jingle.Core (Sequence, Repeat(..), Item(..), Voicing(..), Phonon(..)) where

import GHC.Generics (Generic)

import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))

import Jingle.Types (Note(..))

-- | The top-level structure of a track.
--
-- This consists of a list of either single audible elements or repeated
-- sub-sequences.
type Sequence t a = [Item t a]

-- | A repeated section of a track with endings.
--
-- The '_repEnding' is a possible-empty track sequence that's included in all
-- but the last repetition.
data Repeat t a = Repeat
  { _repContents :: Sequence t a
  , _repEnding :: Sequence t a
  , _repCount :: Int
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic (Repeat t a)

-- | An element of the track sequence: a single note/chord or a repeat.
data Item t a
  = Single t a
  | Rep (Repeat t a)
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic (Item t a)

newtype Voicing = Voicing { _voNotes :: [Note] }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Voicing

data Phonon t ann a = Phonon
  { _phDuration :: t
    -- ^ How long the note sustains (independently of any time-delta).
  , _phAnnotation :: ann
    -- ^ Any bonus information attached to the note (e.g. articulation).
  , _phNote :: a
    -- ^ The base note/voicing itself.
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic (Phonon t ann a)
