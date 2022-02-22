module Main where

import qualified Data.HashSet as HashSet

data CharGuess = Correct Char | Misplaced Char | Incorrect Char
    deriving Show

type WordGuess = [CharGuess]

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- vim: set et
