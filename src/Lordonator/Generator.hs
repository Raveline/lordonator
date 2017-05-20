{-# LANGUAGE OverloadedStrings #-}

module Lordonator.Generator (
  buildSentence
) where

import Prelude hiding (Word, words)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.Random
import Pipes

import Lordonator.Cleaner (Word)
import Lordonator.Model

nextWords :: (MonadRandom m) => [Word] -> Model -> m [Word]
nextWords w m = case M.lookup w (nextWordStat m) of
                  Nothing -> return []
                  Just xs -> fromList xs

randomWord :: (MonadRandom m) => [Word] -> Model -> Producer Word m a
randomWord w m = do newWords <- lift $ nextWords w m
                    if null newWords
                      then yield "" >> randomWord [] m
                      else do let last2Word = drop (length newWords - 2) newWords
                              yield $ T.intercalate " " newWords
                              randomWord last2Word m

sentenceBuilder :: (MonadRandom m) => [Word] -> Int -> Consumer Word m Word
sentenceBuilder fw n = do words <- replicateM n await
                          let w = T.intercalate " " fw
                          return . (`T.snoc` '.') . T.intercalate " " $ (w:words)

-- Since toTitleCase will transform "L'aventure" in "L'Aventure",
-- we need our own method to capitalize only the first letter
singleCapitalize :: T.Text -> T.Text
singleCapitalize t = let initial = (T.toUpper . T.take 1) t
                         rest = T.tail t
                     in T.concat [initial, rest]

buildSentence :: (MonadRandom m) => Int -> Model -> m T.Text
buildSentence n m@(Model _ _ starts d) =
  do firstWords <- fromList starts
     runEffect $ randomWord firstWords m >-> sentenceBuilder firstWords (n `div` d)
