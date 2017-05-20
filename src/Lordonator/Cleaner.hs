-- | Cleaning utilities to make sentences out of a long text
{-# LANGUAGE OverloadedStrings #-}

module Lordonator.Cleaner (
  Word
, Sentences
, asSentences
) where

import Prelude hiding (Word)
import Data.Char (isAlpha)
import qualified Data.Text as T

type Word = T.Text
type Sentences = [Word]

asSentences :: T.Text -> [Sentences]
asSentences =  filter ((>5) . length) . map (filter (not . T.null) . map removeStuff . T.splitOn " ") . onASentenceBasis

onASentenceBasis :: T.Text -> [T.Text]
onASentenceBasis = let sentenceEnding c = c `elem` T.unpack "?!."
                   in T.split sentenceEnding

-- | Normalize strings so they contain only admissible content.
removeStuff :: T.Text -> T.Text
removeStuff = let admissible c = isAlpha c || c `elem` T.unpack "'’-"
              in T.strip . T.filter admissible
