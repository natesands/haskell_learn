--- CHAPTER 9 ---

{-

I/O actions will only be performed when they are given a name of main or when they're inside a bigger I/O action that we composed with a do block. -}

import Data.Char
import Control.Monad

mainA = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <-getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
        
-- continously read a line and print out the same line with the words reversed, ending when
-- a blank line is entered

mainB = do
  line <- getLine
  if null line
    then return ()
    else do
    putStrLn $ reverseWords line
    mainB


reverseWords :: String -> String
reverseWords = unwords . reverse . words

{- in the above, return makes an I/O action out of a pure value... and we need
 "if x the I/O else I/O". it does NOT terminate execution.  whatever is encapsulated into
an IO action using return is discarded unless it is bound to named using '<-' -}

mainC = do
  a <- return "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

{-
putStr like putStrLn but w/out the newline

putChar prints char to terminal

putStr is defined recursively using putChar

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
  putChar x
  putStr xs

-}

{- print function... -}

{- getChar -}


mainE = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    mainE
