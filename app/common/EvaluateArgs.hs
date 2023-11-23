module EvaluateArgs where

import Control.Monad.Random (Random, randomRIO)
import Data.ByteString      (ByteString)
import Data.Digest.CRC32    (crc32)
import Data.String          (fromString)

evaluateArgs
  :: (Num a, Random a, Read a, Num b, Random b, Read b)
  => [String]
  -> IO (a, b)
evaluateArgs args = case args of
  []     -> do
    segment <- randomRIO (0, 3)
    g       <- randomRIO (0,1000000000000)
    return (segment, g)
  [task] ->
    return (0, read task)
  [segment, task] ->
    return (read segment, read task)
  [vnr, manr, matrikel] ->
    return (0, fromIntegral $ crc32 (fromString (vnr ++ show manr ++ matrikel) :: ByteString))
  [segment, vnr, manr, matrikel] ->
    return (read segment, fromIntegral $ crc32 (fromString (vnr ++ show manr ++ matrikel) :: ByteString))
  _      -> error "Too many arguments"
