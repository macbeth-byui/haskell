{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List (group, sort, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)
import System.Environment

import Fmt

-- main :: IO ()
-- main = do
--     [filename] <- getArgs
--     text <- TIO.readFile filename
--     let unique = map head $ group $ sort $ T.words $ T.toLower text
--     TIO.putStrLn $ T.unwords unique
--     print $ length unique

-- Organize information into types
type Entry = (T.Text, Int)
type Vocab = [Entry]

-- extractVocab :: T.Text -> Vocab
-- extractVocab text = 
--     map build $ group $ sort words
--     where
--         words = map cleanWord $ filter (not . T.null) $ T.words text
--         build all@(x:_) = (x, length all)  -- [] not possible
--         cleanWord = T.dropAround (not . isLetter)

-- printAllWords :: Vocab -> IO ()
-- printAllWords vocab = do
--     print "All words:"
--     TIO.putStrLn $ T.unlines $ map fst vocab

-- processTextFile :: FilePath -> IO ()
-- processTextFile filename = do
--     text <- TIO.readFile filename
--     let vocab = extractVocab text
--     printAllWords vocab

-- main :: IO ()
-- main = do
--     [filename] <- getArgs
--     processTextFile filename

-- Use more Pure Functions
allWords :: Vocab -> [Text]
allWords vocab = map fst vocab

wordsCount :: Vocab -> (Int, Int)
wordsCount vocab = (foldl (\ acc x -> acc + snd x) 0 vocab, length vocab)

wordsByFrequency :: Vocab -> Vocab
wordsByFrequency vocab = sortBy (\ (_,a) (_,b) -> compare b a) vocab

extractVocab :: Text -> Vocab
extractVocab text = 
    map custom $ group $ sort wordList
    where
        wordList = map T.toCaseFold $ map cleanWord $ 
            filter (not . T.null) $ T.words text
        custom []        = ("", 0)
        custom l@(x:_) = (x, length l) 
        cleanWord = T.dropAround (not . isLetter)

allWordsReport :: Vocab -> Text
allWordsReport vocab = 
    fmtLn $ nameF "All words:" $ blockListF $ allWords vocab

wordsCountReport :: Vocab -> Text
wordsCountReport vocab = 
    fmtLn $ "Total number of words: " +|total|+ 
            "\nNumber of unique words: " +|unique|+ ""
    where
        (total, unique) = wordsCount vocab


frequentWordsReport :: Vocab -> Int -> Text
frequentWordsReport vocab count = 
    fmtLn $ nameF "Frequent Words:" $ blockListF' "-" custom top
    where 
        top = take count $ wordsByFrequency vocab
        custom (a, b) = "" +|a|+ ": " +|b|+ ""

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile filename printAll topCount = do
    text <- TIO.readFile filename
    let vocab = extractVocab text
    when printAll $ TIO.putStrLn $ allWordsReport vocab
    TIO.putStrLn $ wordsCountReport vocab
    TIO.putStrLn $ frequentWordsReport vocab topCount


main :: IO ()
main = do
    TIO.putStrLn "hello"
    [filename] <- getArgs
    processTextFile filename True 10
