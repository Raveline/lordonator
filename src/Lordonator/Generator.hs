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

nextWords :: (MonadRandom m) => Word -> Model -> m [Word]
nextWords w m = case M.lookup w (nextWordStat m) of
                  Nothing -> fmap (:[]) . fromList $ sentenceStarter m
                  Just xs -> fromList xs

randomWord :: (MonadRandom m) => Word -> Model -> Producer Word m a
randomWord w m = do newWords <- lift $ nextWords w m
                    let lastWord = last newWords
                    yield $ T.intercalate " " newWords
                    randomWord lastWord m

sentenceBuilder :: (MonadRandom m) => Word -> Int -> Consumer Word m Word
sentenceBuilder w n = do words <- replicateM n await
                         return . (`T.snoc` '.') . T.intercalate " " $ (w:words)

buildSentence :: (MonadRandom m) => Int -> Model -> m T.Text
buildSentence n m@(Model _ _ starts d) =
  do firstWord <- fromList starts
     runEffect $ randomWord firstWord m >-> sentenceBuilder (T.toTitle firstWord) (n `div` d)
