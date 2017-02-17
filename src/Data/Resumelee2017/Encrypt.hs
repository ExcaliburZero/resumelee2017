module Data.Resumelee2017.Encrypt where

import qualified Data.ByteString as BS
import Control.Monad (zipWithM_)
import Data.List.Split (chunksOf)
import Data.Word

-- $setup
-- >> import Test.QuickCheck

-- | Encrypts the given file and splits it into the given number of parts.
encryptFile :: FilePath -> Int -> IO ()
encryptFile file pieces = do
  _ <- putStrLn $ "Cryptifying file: " ++ file
  contents <- BS.unpack <$> BS.readFile file
  let invContents = map invertByte contents
  let broken      = breakUpFile invContents pieces
  let rBroken     = fmap (map reverse) broken
  _ <- case rBroken of
    Just a  -> writeParts file a pieces
    Nothing -> return ()
  return ()

-- | Writes the given parts to files.
writeParts :: FilePath -> [[Word8]] -> Int -> IO ()
writeParts file parts pieces = do
  let bParts = map BS.pack parts
  let fNames = map (((file ++ ".part") ++) . show) [1..pieces]
  zipWithM_ BS.writeFile fNames bParts

-- | Breaks up the given String into n number of pieces.
--
-- >>> breakUpFile "12" 2
-- Just ["1","2"]
-- >>> breakUpFile "12345" 2
-- Just ["123","45"]
breakUpFile :: [a] -> Int -> Maybe [[a]]
breakUpFile f n
    | n > 0 && fLength > 0 = Just chunks
    | otherwise            = Nothing
  where
    chunks  = chunksOf nChunks f
    nChunks = (fLength `div` n) + (if fLength `mod` n == 0 then 0 else 1)
    fLength = length f

-- | Inverts the given byte.
--
-- >>> invertByte 0
-- 255
-- 
-- prop> \w -> invertByte w /= w 
-- prop> \w -> invertByte (invertByte w) == w
invertByte :: Word8 -> Word8
invertByte byte = 255 - byte
