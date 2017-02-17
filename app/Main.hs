module Main where

import System.Environment (getArgs)

import Data.Resumelee2017.Encrypt (processFile)

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  processFile file
