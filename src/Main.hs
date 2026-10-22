{-
Usage:
  tm FILE
    Parses and tests the Turing machine in FILE against every word under a certain length
    in [012]* for the language {0ⁿ1ⁿ2ⁿ | n : ℕ}.
-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment

import TM

main :: IO ()
main = do
  file:_ <- getArgs
  src <- T.readFile file
  checkTm False file src
  putStrLn "All OK!"
