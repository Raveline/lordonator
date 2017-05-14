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
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Applicative((<|>))

import Lordonator.Cleaner

-- AKA bigrams
type WordProba = [(Word, Rational)]
-- Markovish series of consecutive words
type WordsProba = [([Word], Rational)]
type ModelBuilder = M.Map Word [[Word]]
type NextWordProba = M.Map Word WordsProba

data Model = Model { nextWordStat :: NextWordProba
                   , sentenceEnder :: WordProba
                   , sentenceStarter :: WordProba
                   , depth :: Int }

-- | Ugly manually-recursive function that will
-- break a list into sublist of length n.
--
-- Example:
--
-- >>> withDepth 4 ["He", "can't", "read", "my", "Poker", "face"]
-- [["He","can't","read","my"],["can't","read","my","Poker"],["read","my","Poker","face"]]
withDepth :: Int -> [a] -> [[a]]
withDepth _ [] = []
withDepth n (w:ws)
  | length ws < (n-1) = []
  | otherwise = (w:take (n - 1) ws):withDepth n ws

-- | Train a model from an example.
-- Entry point to this submodule.
train :: Int -> T.Text -> Model
train n t = let sentences = asSentences t
                starters = stats $ map head sentences
                enders = stats $ map last sentences
                sequences = map (withDepth n) sentences
                builder = (foldl . foldl) putWord M.empty sequences
          in Model (M.map stats builder) enders starters 4

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
putWord :: ModelBuilder -> [Word] -> ModelBuilder
putWord model (w:ws) = let insertion v = (ws :) <$> (v <|> Just [])
                       in M.alter insertion w model
putWord m _ = m
