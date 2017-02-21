module Data.Resumelee2017.Encrypt where

import qualified Data.ByteString as BS
import Control.Monad (zipWithM_)
import Data.List.Split (chunksOf)
import Data.Word

-- $setup
-- >>> import Test.QuickCheck

-- | Encrypts the given file and splits it into the given number of parts.
encryptFile :: FilePath -> Int -> IO ()
encryptFile file pieces = do
  _ <- putStrLn $ "Encrypting file: " ++ file
  contents <- BS.unpack <$> BS.readFile file
  let invContents = map invertByte contents
  let broken      = breakUpFile invContents pieces
  let rBroken     = fmap (map reverse) broken
  case rBroken of
    Just a  -> writeParts file a pieces
    Nothing -> return ()

-- | Decrypts the given file from its parts.
decryptFile :: FilePath -> Int -> IO ()
decryptFile file pieces = do
  _ <- putStrLn $ "Decrypting file: " ++ file
  eContents <- readParts file pieces
  let broken = map reverse eContents
  let invContents = concat broken
  let parts = map invertByte invContents
  let contents = BS.pack parts
  BS.writeFile file contents

-- | Generates the piece file names for the given file and number of pieces.
-- 
-- >>> pieceNames "a.txt" 2
-- ["a.txt.part1","a.txt.part2"]
--
-- prop> \(s, n) -> if n > -1 then length (pieceNames s n) == n else True
pieceNames :: FilePath -> Int -> [FilePath]
pieceNames file pieces = map (((file ++ ".part") ++) . show) [1..pieces]

-- | Writes the given parts to files.
writeParts :: FilePath -> [[Word8]] -> Int -> IO ()
writeParts file parts pieces = do
  let bParts = map BS.pack parts
  let fNames = pieceNames file pieces
  zipWithM_ BS.writeFile fNames bParts

-- | Reads in the part files.
readParts :: FilePath -> Int -> IO [[Word8]]
readParts file pieces = do
  let fileNames = pieceNames file pieces
  files <- mapM BS.readFile fileNames
  return $ map BS.unpack files

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
