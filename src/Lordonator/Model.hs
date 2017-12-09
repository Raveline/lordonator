-- | A naive Markov-like model generator. The core concept is fairly
-- simple : we take a text, try to clean it up, and make basic statistics
-- to know what words tend to follow others.
{-# LANGUAGE OverloadedStrings #-}

module Lordonator.Model (
  Word
, WordProba
, WordsProba
, Model (..)
, NextWordProba
, train
) where

import Prelude hiding (Word, words)
import qualified Data.Text as T
import Data.List (tails)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Applicative((<|>))

import Lordonator.Cleaner

-- AKA bigrams
type WordProba = [(Word, Rational)]
-- Markovish series of consecutive words
type WordsProba = [([Word], Rational)]
type ModelBuilder = M.Map [Word] [[Word]]
type NextWordProba = M.Map [Word] WordsProba

data Model = Model { nextWordStat :: NextWordProba
                   , sentenceStarter :: WordsProba
                   , ngrams :: Int
                   , sequences :: Int}

-- | Break a list into sublists of length n.
--
-- Example:
--
-- >>> withDepth 4 ["He", "can't", "read", "my", "Poker", "face"]
-- [["He","can't","read","my"],["can't","read","my","Poker"],["read","my","Poker","face"]]
withDepth :: Int -> [a] -> [[a]]
withDepth n = takeWhile ((== n) . length) . fmap (take n) . tails

-- | Train a model from an example.
-- Entry point to this submodule.
train :: Int -> Int -> T.Text -> Model
train u d t = let sentences = asSentences t
                  starters = stats $ map (take u) sentences
                  sequences = (withDepth (u + d)) (T.splitOn " " t)
                  builder = foldl (putWord u) M.empty sequences
              in Model (M.map stats builder) starters u d

-- | Map a list of words to a list of probabilities
-- of occurrences for each word, depending on the
-- number of time a word showed up.
stats :: (Ord a) => [a] -> [(a, Rational)]
stats txts = let num = length txts
                 uniques = S.fromList txts
                 count w = length . filter (w ==) $ txts
                 prob w = (w, (toRational . count $ w) / toRational num)
             in S.toList . S.map prob $ uniques

-- | Utility function to build up our model. Given a pair
-- of two consecutive words, add the 2nd one to the list of
-- words that tend follow the first one.
putWord :: Int -> ModelBuilder -> [Word] -> ModelBuilder
putWord u model ws = let key = take u ws
                         val = drop u ws
                         insertion v = (val :) <$> (v <|> Just [])
                     in M.alter insertion key model
