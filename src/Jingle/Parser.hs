{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Jingle.Parser (comp) where

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
import Jingle.Types (Score(..), Track(..))

type Parser = Parsec Void Text

noteName :: Parser NoteName
noteName = choice $ zipWith (($>) . char) ['A'..'G'] [A .. G]

accidental :: Parser Accidental
accidental = choice
  [ DoubleSharp <$ string "##"
  , Sharp <$ char '#'
  , DoubleFlat <$ string "bb"
  , Flat <$ char 'b'
  , Natural <$ char 'n'
  ]

note :: Parser Note
note = Named <$> optional decimal <*> noteName <*> optional accidental

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

chord :: Parser (Chord Note)
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

playPhonon :: Parser (Phonon Rational (Articulated (Chord Note)))
playPhonon = do
  v <- chord
  dur <- duration
  art <- optional articulation
  return $ Phonon dur (Just $ Articulated v art)

phonon :: Parser (Phonon Rational (Articulated (Chord Note)))
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

comp :: Parser (Score TrackContents)
comp =
  Score
    <$> (decimal <* symbol ws ";")
    <*> sepBy1 track (symbol ws ";")
    <* eof
