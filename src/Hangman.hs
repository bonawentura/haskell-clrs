module Hangman where

import Control.Monad.State.Lazy

{-
do an infinite loop until a condition is met, then display the accumulated state

    take a single line
    if line contains a single char, check against blacklist
        if not on blacklist, add to acc
        else display current acc and terminate loop

 -}
type Guesses = String

data GameState = GameState
  { guesses :: Guesses,
    lives :: Int,
    blacklist :: String
  }

initialState = GameState {guesses = "", lives = 99, blacklist = "qwerty"}

-- -- istat' :: Char -> State GameState ()
-- istat' c = state $ \gs -> ((), gs {guesses = c : guesses gs})

-- processGuess :: State GameState -> String -> State GameState ()
-- processGuess gs guess =
