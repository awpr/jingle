{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A normalized, ascetic representation of synthesizer-bound music.
--
-- This differs from the higher-level abstract syntax in that it dispenses with
-- all the syntactic sugar, notational shorthands, and compromises to
-- user-facing convenience, in favor of simplicity and flexibility.
--
-- In order to give the user-facing syntax and parser more flexibility, we
-- tolerate some duplication between the Core and AST datatypes.

module Jingle.Core
  ( Sequence(..), Repeat(..), Item(..)
  , Voicing(..)
  , Phonon(..), phDuration, phAnnotation, phNote
  , TrackContents
  ) where

import Data.Bifunctor (first)
import GHC.Generics (Generic)

import Control.Lens (makeLenses)
import Data.Portray (Portray(..), Portrayal(..), PortrayDataCons(..))
import Data.Wrapped (Wrapped(..))
import Data.EventList.Relative.TimeTime qualified as EventList

import Jingle.Types (Note(..))

toList :: EventList.T t a -> ([(t, a)], t)
toList el = case EventList.viewL el of
  (dt, Nothing) -> ([], dt)
  (dt, Just (x, el')) -> first ((dt, x):) $ toList el'

portrayEventList :: (Portray t, Portray a) => EventList.T t a -> Portrayal
portrayEventList el =
  let (xs, dt) = toList el
  in  case xs of
        [] -> Apply (Name "pause") [portray dt]
        _  -> Apply (Name "fromList") [portray xs, portray dt]

-- | The top-level structure of a track.
--
-- This consists of a list of either single audible elements or repeated
-- sub-sequences.
newtype Sequence t a = Sequence { getItems :: EventList.T t (Item t a) }
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid)

instance (Portray t, Portray a) => Portray (Sequence t a) where
  portray (Sequence x) = Apply (Name "Sequence") [portrayEventList x]

-- | A repeated section of a track with endings.
--
-- The '_repEnding' is a possible-empty track sequence that's included in all
-- but the last repetition.
data Repeat t a = Repeat
  { _repContents :: Sequence t a
  , _repEnding :: Sequence t a
  , _repCount :: Int
  }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via PortrayDataCons (Repeat t a)

-- | An element of the track sequence: a single note/chord or a repeat.
data Item t a
  = Single a
  | Rep (Repeat t a)
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via Wrapped Generic (Item t a)

newtype Voicing = Voicing { _voNotes :: [Note] }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via PortrayDataCons Voicing

data Phonon t ann a = Phonon
  { _phDuration :: t
    -- ^ How long the note sustains (independently of any time-delta).
  , _phAnnotation :: ann
    -- ^ Any bonus information attached to the note (e.g. articulation).
  , _phNote :: a
    -- ^ The base note/voicing itself.
  }
  deriving (Generic, Eq, Ord, Show)
  deriving Portray via PortrayDataCons (Phonon t ann a)

$(makeLenses ''Phonon)

type TrackContents t ann a = Sequence t (Phonon t ann a)
