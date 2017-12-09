{-# LANGUAGE OverloadedStrings #-}
module Main where


import Prelude hiding (Word)
import Control.Monad (replicateM)
import Data.Semigroup ((<>))

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (toList)
import Options.Applicative
import Options.Applicative.Types

import Lordonator.Model
import Lordonator.Generator
import Paths_lordonator

type SentenceLength = Int
type SentencesNum = Int
type Depth = Int

data Command = Generate { source :: Maybe FilePath,
                          numSentences :: SentencesNum,
                          upstream :: Int,
                          downstream :: Int,
                          dry :: Bool }

sourceParser :: ReadM (Maybe FilePath)
sourceParser = Just <$> readerAsk

argsParser :: Parser Command
argsParser = Generate <$> option sourceParser
                                     (long "source"
                                    <> metavar "FILE"
                                    <> help "Source file to generate content"
                                    <> value Nothing)
                      <*> option auto (long "sentences"
                                    <> short 's'
                                    <> metavar "INT"
                                    <> value 1
                                    <> help "Number of sentences to generate")
                      <*> option auto (long "up"
                                    <> short 'u'
                                    <> metavar "INT"
                                    <> value 2
                                    <> help "How many upstream words we are using to train the model. E.g., n-grams used.")
                      <*> option auto (long "down"
                                     <> short 'o'
                                     <> metavar "INT"
                                     <> value 5
                                     <> help "How many downstream words we are using following the upstream n-gram to train the model.")
                      <*> switch (long "dry"
                                <> short 'd'
                                <> help "Do not generate text but print the trained model")

main :: IO ()
main = let parser = info (argsParser <**> helper)
             (fullDesc
             <> progDesc "Generate random texts from a source"
             <> header "Lordonator - A Markovish random text generator")
           go (Generate s _ u d True) = getModel u d s >>= printModel
           go (Generate s senNum u d _) = getModel u d s >>= generate senNum
       in go =<< execParser parser

getModel :: Int -> Int -> Maybe FilePath -> IO Model
getModel u d fp = train u d <$> (maybe (getDataFileName "lordon.txt") pure fp >>= TIO.readFile)

generate :: SentencesNum -> Model -> IO ()
generate senNum m = do sents <- replicateM senNum . buildSentence $ m
                       mapM_ TIO.putStrLn sents

separator :: T.Text -> IO ()
separator t = TIO.putStrLn "**********************"
  >> TIO.putStrLn t
  >> TIO.putStrLn "**********************"

printModel :: Model -> IO ()
printModel m =
  let dispProba (sub, pb) = T.concat [ T.intercalate " " sub
                                     , " --- "
                                     , T.pack . show $ pb ]
      wordstats = toList . nextWordStat $ m
      dispNextWordStats w wp = TIO.putStrLn "--------------"
                               >> TIO.putStrLn (T.toUpper . T.intercalate " " $ w)
                               >> mapM_ (TIO.putStrLn . dispProba) wp
  in separator "Bigrams & next sequences"
     >> mapM_ (uncurry dispNextWordStats) wordstats
     >> separator "Sentence starters"
     >> mapM_ (TIO.putStrLn . dispProba) (sentenceStarter m)
