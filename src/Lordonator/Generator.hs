{-# LANGUAGE OverloadedStrings #-}
module Lordonator.Generator (
  buildSentence
  , randomWord
) where

import Prelude hiding (Word, words, takeWhile)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.Random
import Pipes
import qualified Pipes.Prelude as P

import Lordonator.Cleaner (Word)
import Lordonator.Model

nextWords :: (MonadRandom m) => [Word] -> Model -> m [Word]
nextWords w m = case M.lookup w (nextWordStat m) of
                  Nothing -> return []
                  Just xs -> fromList xs

randomWord :: (MonadRandom m) => [Word] -> Model -> Producer [Word] m ()
randomWord w m = do newWords <- lift $ nextWords w m
                    if null newWords
                      then yield [] >> randomWord [] m
                      else do let last2Word = drop (length newWords - 2) newWords
                              yield $ newWords
                              randomWord last2Word m


display :: (MonadIO m) => [Word] -> Consumer [Word] m Word
display fw = do x <- await
                if null x
                  then return $ T.intercalate " " fw
                  else display $ x ++ fw


-- Since toTitleCase will transform "L'aventure" in "L'Aventure",
-- we need our own method to capitalize only the first letter
singleCapitalize :: T.Text -> T.Text
singleCapitalize t = let initial = (T.toUpper . T.take 1) t
                         rest = T.tail t
                     in T.concat [initial, rest]

buildSentence :: (MonadRandom m, MonadIO m) => Model -> m Word
buildSentence m@(Model _ starts d) =
  do fw <- fromList starts
     sentenceBits <- P.toListM (randomWord fw m >-> P.takeWhile (not . null))
     return $  singleCapitalize . flip T.snoc '.' . T.intercalate " " . concat $ sentenceBits
