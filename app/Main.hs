module Main where

import System.Environment (getArgs)

import Data.Resumelee2017.Encrypt (encryptFile)

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  let pieces = read (args !! 1) :: Int
  encryptFile file pieces
