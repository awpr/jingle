{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Jingle.Parser (comp) where

import Data.Char (ord)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.Void (Void)

import Data.Text (Text)
import Data.Vector qualified as V
import Text.Megaparsec
import Text.Megaparsec.Char (char, string, space)
import Text.Megaparsec.Char.Lexer (decimal, lexeme, symbol)

import Jingle.Syntax
import Jingle.Types (Comp(..), Track(..))

type Parser = Parsec Void Text

note :: Parser Note
note =
  do
    octave <- option 4 decimal
    nm <- satisfy (\x -> x >= 'A' && x <= 'G')
    acc <- choice
      [ 1 <$ char '#'
      , -1 <$ char 'b'
      , pure 0
      ]
    pure $ Note $ 12*octave + (notes V.! (ord nm - ord 'A')) + acc
 where
  -- Starting from A, how many semitones is each note above the same-octave C?
  --
  -- The numbers are weird because the octave numbering system itself is weird.
  -- I don't make the rules.
  notes = V.fromList [9, 11, 0, 2, 4, 5, 7]

flag :: Parser a -> Parser Bool
flag = fmap isJust . optional

chordQuality :: Parser ChordQuality
chordQuality = choice
  [ Fifth <$ char '5'
  , Maj <$ char 'M' <*> flag (char '7')
  , HalfDim <$ string "m7b5"
  , Min <$ char 'm' <*> flag (char '7')
  , Dom <$ char '7'
  , Aug <$ string "+" <*> flag (char '7')
  , Dim <$ string "dim" <*> flag (char '7')
  ]

interval :: Parser Interval
interval = decode <$> decimal
 where
  semitones = V.fromList [0, 2, 4, 5, 7, 9, 11]
  decode n =
    let (octave, itvMinus1) = divMod (n - 1) 7
    in  Interval $ 12 * octave + semitones V.! itvMinus1

chord :: Parser Chord
chord =
  Chord
    <$> (note <?> "note name")
    <*> (optional chordQuality <?> "chord quality")
    <*> many (string "add" *> interval <?> "added note")

articulation :: Parser Articulation
articulation = choice
  [ "." $> Staccato
  , "^" $> Marcato
  , ">" $> Accent
  , "-" $> Tenuto
  , "&" $> Legato
  ]

-- "" => 1; ":3" => 3; ":1/3" => 1/3
duration :: Parser Rational
duration =
  (%)
    <$> option 1 (char ':' *> decimal)
    <*> option 1 (char '/' *> decimal)

ws :: Parser ()
ws = space

playPhonon :: Parser (Phonon Rational (Articulated Chord))
playPhonon = do
  v <- chord
  dur <- duration
  art <- optional articulation
  return $ Phonon dur (Just $ Articulated v art)

phonon :: Parser (Phonon Rational (Articulated Chord))
phonon = choice
  [ lexeme ws playPhonon
  , lexeme ws $ char '_' *> (Phonon <$> duration <*> pure Nothing)
  ]

rep :: Parser Repeat
rep = do
  _ <- symbol ws "|:"
  contents <- many trackPiece
  ending <- option [] $ symbol ws "|" *> many trackPiece
  _ <- symbol ws ":|"
  n <- option 2 (char 'x' *> lexeme ws decimal)
  return $ Repeat contents ending n

grp :: Parser TrackPiece
grp = do
  _ <- lexeme ws (char '(')
  contents <- many trackPiece
  lexeme ws $ do
    _ <- char ')'
    dur <- duration
    art <- optional articulation
    return $ Group contents dur art

trackPiece :: Parser TrackPiece
trackPiece =
  choice
    [ Single <$> (flip Advance <$> phonon <*> (not <$> flag (symbol ws ",")))
    , grp
    , Rep <$> rep
    ]

track :: Parser (Track TrackContents)
track =
  Track
    <$> (option "" $ lexeme ws $ char '<' *> takeWhileP Nothing (/= '>') <* char '>')
    <*> many trackPiece

comp :: Parser (Comp TrackContents)
comp =
  Comp
    <$> (decimal <* symbol ws ";")
    <*> sepBy1 track (symbol ws ";")
    <* eof
