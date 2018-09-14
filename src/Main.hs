{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Control.Monad
import Data.Aeson
import Data.Char (toLower)
import Data.Function (on)
import Euterpea
import Euterpea.Music
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  [path] <- getArgs
  p@(Piece pTempo pLines) <- do
    lbs <- LBS.readFile path
    case eitherDecode lbs of
      Left e -> do
        error $ "File '" <> path <> "': " <> e
      Right piece -> pure piece
  print p
  play $ chord $
    fmap (playTrack pTempo) pLines
  where
    playTrack t l =
      changeInstrument (tInstrument l) $ tempo t $
        (line $ fmap vbToNote (drop (tDelay l) (earth (tBeats l) (tTotal l))))
      where
        vbToNote Void = rest qn
        vbToNote Beat = note qn (tNote l, tOctave l)

data VB = Void | Beat deriving (Show)

earth :: Int -> Int -> [VB]
earth beats total = L.cycle $
  aux (replicate beats [Beat]) (replicate (total - beats) [Void])
  where
    aux :: ()
      => [[VB]] -- ^ sequences
      -> [[VB]] -- ^ remaining
      -> [VB]
    aux sequences [] = join sequences
    aux sequences [rem] = join (sequences ++ [rem])
    aux sequences remaining =
      let minLength  = length sequences `min` length remaining
          (seqFront, seqBack) = splitAt minLength sequences
          (remFront, remBack) = splitAt minLength remaining
          sequences' = zipWith (++) seqFront remFront
          remaining' = seqBack ++ remBack
      in aux sequences' remaining'

instruments :: [InstrumentName]
instruments = [AcousticGrandPiano .. Gunshot]

notes :: [PitchClass]
notes = [Cff .. Bss ]

unshow :: (Show a) => [a] -> String -> Maybe a
unshow values s =
  L.find
    ((`eq` (filter (/= ' ') s)) . show)
    values
  where
    eq = (==) `on` fmap toLower

data Track = Track
  { tInstrument :: InstrumentName
  , tNote   :: PitchClass
  , tOctave :: Octave
  , tDelay  :: Int
  , tBeats  :: Int
  , tTotal  :: Int }
  deriving (Show, Eq, Ord)

instance FromJSON Track where
  parseJSON = withObject "Track" $ \v -> Track
    <$> (v .: "instrument" >>= parseInstrument)
    <*> (v .: "note" >>= parseNote )
    <*> v .: "octave"
    <*> v .: "delay"
    <*> v .: "beats"
    <*> v .: "total"
    where
      parseInstrument t = case unshow instruments s of
        Just instrument -> pure instrument
        Nothing -> fail ("'" <> s <> "' is not a valid instrument")
        where s = T.unpack t
      parseNote t = case unshow notes s of
        Just note -> pure note
        Nothing -> fail ("'" <> s <> "' is not a valid note")
        where s = T.unpack t

data Piece = Piece
  { pTempo  :: Rational
  , pTracks :: [Track] }
  deriving (Show)

instance FromJSON Piece where
  parseJSON = withObject "Piece" $ \v -> Piece
    <$> v .: "tempo"
    <*> v .: "tracks"
