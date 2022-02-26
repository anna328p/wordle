{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (Word)

import System.Environment
import System.IO

import Data.List
import qualified Data.Text as Text
import qualified Data.HashSet as HS

import Data.Hashable

import qualified Data.Time as T

import Rainbow as R

-- Types

newtype Word = Word String
    deriving (Show, Ord, Eq, Hashable)

type Lexicon = HS.HashSet Word

data Letter = Correct Char | Misplaced Char | Incorrect Char    deriving (Show, Eq)
type Guess = [Letter]

data RoundResult = ValidGuess | DuplicateGuess | Found | OutOfGuesses
data GameResult = Win [Guess] | Fail

-- An alphabet is the set of available and guessed letters shown to the user
data Key = Guessed Letter | Unused Char                         deriving (Show, Eq)
type Alphabet = [Key]

wordString :: Word -> String
wordString (Word str) = str

-- Basic operations on guessed letters

lchar :: Letter -> Char
lchar (Correct ch)   = ch
lchar (Misplaced ch) = ch
lchar (Incorrect ch) = ch

charChunk :: Char -> R.Chunk
charChunk = R.chunk . Text.singleton

letterChunk :: Letter -> R.Chunk
letterChunk = charChunk . lchar

letterColors :: Letter -> (R.Radiant, R.Radiant)
letterColors (Correct _)   = (R.black, R.green)
letterColors (Misplaced _) = (R.black, R.yellow)
letterColors (Incorrect _) = (R.white, R.black)

fmtLetter :: Letter -> R.Chunk
fmtLetter lv = R.fore fcolor $ R.back bcolor lchunk
    where
        lchunk = letterChunk lv
        (fcolor, bcolor) = letterColors lv

fmtGuess :: Guess -> [R.Chunk]
fmtGuess = map fmtLetter

findLetter :: Guess -> Char -> Maybe Letter
findLetter guess char = find (\lv -> char == lchar lv) guess

-- Displaying and updating alphabets

keyChar :: Key -> Char
keyChar (Guessed letter) = lchar letter
keyChar (Unused char)    = char

updateKey :: Guess -> Key -> Key
updateKey guess key =
    case findLetter guess $ keyChar key of
        Nothing -> key
        Just gl -> case key of
            Unused char       -> Guessed gl
            Guessed kl -> case (kl, gl) of
                (Correct _,   _)         -> Guessed kl
                (Misplaced _, Correct _) -> Guessed gl
                (Misplaced _, _)         -> Guessed kl
                (_,           _)         -> Guessed gl

updateAlphabet :: Alphabet -> Guess -> Alphabet
updateAlphabet super guess = map (updateKey guess) super

defaultAlphabet :: Alphabet
defaultAlphabet = map Unused ['a'..'z']

alphabet :: [Guess] -> Alphabet
alphabet = foldl' updateAlphabet defaultAlphabet

fmtKey :: Key -> Chunk
fmtKey (Guessed lv) = fmtLetter lv
fmtKey (Unused ch)  = R.back R.grey $ charChunk ch

fmtAlphabet :: Alphabet -> [Chunk]
fmtAlphabet = map fmtKey

-- Guessing logic

guessLetter :: String -> Char -> Char -> Letter
guessLetter word target guess
    | guess == target   = Correct guess
    | guess `elem` word = Misplaced guess
    | otherwise         = Incorrect guess

evalGuess :: Word -> Word -> Guess
evalGuess target guess = zipWith (guessLetter targetStr) targetStr guessStr
    where
        targetStr = wordString target
        guessStr  = wordString guess

-- Game logic

parseWord :: Lexicon -> String -> Maybe Word
parseWord lexicon input
    | HS.member (Word input) lexicon = Just $ Word input
    | otherwise                      = Nothing

gameRound :: Word -> [Guess] -> Word -> RoundResult
gameRound target prevGuesses guessedWord
    | guessedWord == target    = Found
    | length prevGuesses >= 5  = OutOfGuesses
    | guess `elem` prevGuesses = DuplicateGuess
    | otherwise                = ValidGuess
    where guess = evalGuess target guessedWord

playGame' :: Lexicon -> Word -> [Guess] -> IO GameResult
playGame' lexicon target guesses = do
    putChunks $ fmtAlphabet $ alphabet guesses
    putStr " "
    guessedWord <- getGuess lexicon

    let guess = evalGuess target guessedWord
    let newGuesses = guesses ++ [guess]

    case gameRound target guesses guessedWord of
        ValidGuess     -> do
            putChunksLn $ fmtGuess guess
            playGame' lexicon target newGuesses
        DuplicateGuess -> do
            putStrLn "Duplicate guess."
            playGame' lexicon target guesses
        Found          -> do
            putChunksLn $ fmtGuess guess
            return $ Win newGuesses
        OutOfGuesses   -> do
            return Fail

getGuess :: Lexicon -> IO Word
getGuess lexicon = do
    putStr "Guess: "
    hFlush stdout

    line <- getLine
    case parseWord lexicon line of
        Just x -> return x
        Nothing -> do
            putStrLn "Invalid guess. Try again."
            getGuess lexicon

playGame :: Lexicon -> Word -> IO GameResult
playGame lexicon target = playGame' lexicon target []

cst :: T.TimeZone
cst = read "CST"

getDayNumber :: IO Int
getDayNumber = do
    currTime <- T.getCurrentTime
    let day = T.toModifiedJulianDay $ T.localDay $ T.utcToLocalTime cst currTime

    return $ fromIntegral $ day - 59383

guessCountMessage :: Int -> String
guessCountMessage n = case n of
    1 -> "Genius"
    2 -> "Magnificent"
    3 -> "Impressive"
    4 -> "Splendid"
    5 -> "Great"
    6 -> "Phew"
    _ -> "Out of range (this should never happen)"

fmtShareLetter :: Letter -> String
fmtShareLetter letter = case letter of
    Correct _ -> "🟩"
    Misplaced _ -> "🟨"
    Incorrect _ -> "⬜"

fmtShareGuess :: Guess -> String
fmtShareGuess guess = concatMap fmtShareLetter guess ++ "\n"

fmtShareGuesses :: Int -> [Guess] -> String
fmtShareGuesses day guesses =
    "Wordle " ++ show day ++ " " ++ show (length guesses) ++ "/6\n" ++
    concatMap fmtShareGuess guesses

main :: IO ()
main = do
    dailyWordList <- readFile "data/daily_words.txt"
    otherWordList <- readFile "data/other_words.txt"

    let dailyWords = lines dailyWordList
    let otherWords = lines otherWordList
    let allWords = map Word $ dailyWords ++ otherWords

    let lexicon = HS.fromList allWords

    args <- getArgs
    currentDay <- getDayNumber
    let dayNum = if null args then currentDay else read $ head args

    let target = Word $ dailyWords !! (dayNum - 1)

    res <- playGame lexicon target
    case res of
        Win guesses -> do
            putStrLn $ guessCountMessage $ 1 + length guesses
            putStrLn ""
            putStrLn $ fmtShareGuesses dayNum guesses
        Fail -> do
            putStrLn "Unsuccessful"


-- vim: et :
