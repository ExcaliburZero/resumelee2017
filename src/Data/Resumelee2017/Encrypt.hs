module Data.Resumelee2017.Encrypt where

import Data.List.Split (chunksOf)

processFile :: FilePath -> IO ()
processFile file = do
  _ <- putStrLn $ "Cryptifying file: " ++ file
  return ()

-- | Breaks up the given String into n number of pieces.
--
-- >>> breakUpFile 2 "12"
-- Just ["1","2"]
-- >>> breakUpFile 2 "12345"
-- Just ["123","45"]
breakUpFile :: Int -> String -> Maybe [String]
breakUpFile n f
    | n > 0 && fLength > 0 = Just chunks
    | otherwise            = Nothing
  where
    chunks  = chunksOf nChunks f
    nChunks = (fLength `div` n) + (if fLength `mod` n == 0 then 0 else 1)
    fLength = length f


