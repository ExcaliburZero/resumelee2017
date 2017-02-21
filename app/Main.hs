module Main where

import Console.Options
import Text.Read

import Data.Resumelee2017.Encrypt (decryptFile, encryptFile)

main :: IO ()
main = defaultMain $ do
  command "encrypt" $ do
    input <- argument "input_file" Right
    pieces <- argument "pieces" readInt
    action $ \toParam -> encryptFile (toParam input) (toParam pieces)
  command "decrypt" $ do
    input <- argument "input_file" Right
    pieces <- argument "pieces" readInt
    action $ \toParam -> decryptFile (toParam input) (toParam pieces)

readInt :: String -> Either String Int
readInt s = case readMaybe s :: Maybe Int of
  Just a  -> Right a
  Nothing -> Left $ s ++ " is not an integer."
