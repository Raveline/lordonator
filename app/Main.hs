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
                          size :: SentenceLength,
                          numSentences :: SentencesNum,
                          depth :: Int,
                          dry :: Bool }

sourceParser :: ReadM (Maybe FilePath)
sourceParser = Just <$> readerAsk

argsParser :: Parser Command
argsParser = Generate <$> option sourceParser
                                     (long "source"
                                    <> metavar "FILE"
                                    <> help "Source file to generate content"
                                    <> value Nothing)
                      <*> option auto (long "length"
                                    <> short 'l'
                                    <> metavar "INT"
                                    <> value 12
                                    <> help "Ideal number of words per sentence to generate")
                      <*> option auto (long "sentences"
                                    <> short 's'
                                    <> metavar "INT"
                                    <> value 1
                                    <> help "Number of sentences to generate")
                      <*> option auto (long "depth"
                                    <> short 'e'
                                    <> metavar "INT"
                                    <> value 5
                                    <> help "Depth of training (i.e., number of n-grams for training)")
                      <*> switch (long "dry"
                                <> short 'd'
                                <> help "Do not generate text but print the trained model")

main :: IO ()
main = let parser = info (argsParser <**> helper)
             (fullDesc
             <> progDesc "Generate random texts from a source"
             <> header "Lordonator - A Markovish random text generator")
           go (Generate s _ _ d True) = getModel d s >>= mapM_ (uncurry printModel) . toList . nextWordStat
           go (Generate s senLen senNum d _) = getModel d s >>= generate senNum senLen
       in go =<< execParser parser

getModel :: Depth -> Maybe FilePath -> IO Model
getModel n fp = train n <$> (maybe (getDataFileName "lordon.txt") pure fp >>= TIO.readFile)

generate :: SentencesNum -> SentenceLength -> Model -> IO ()
generate senNum senLen m = do sents <- replicateM senNum . buildSentence senLen $ m
                              mapM_ TIO.putStrLn sents

printModel :: Word -> WordsProba -> IO ()
printModel w wp = let dispProba (sub, pb) = T.concat [ T.intercalate " " sub
                                                     , " --- "
                                                     , T.pack . show $ pb ]
                  in TIO.putStrLn "--------------"
                     >> TIO.putStrLn (T.toUpper w)
                     >> mapM_ (TIO.putStrLn . dispProba) wp
