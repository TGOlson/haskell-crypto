module Crypto where

import System.Directory
import Control.Monad
import qualified Data.ByteString as BS
import Crypto.HashTree

main :: IO ()
main = do
    unlessM (doesFileExist filePath) $ do
        putStrLn "Data file does not exist. Writing new file..."
        writeTestFile

    testData <- BS.readFile filePath

    let tree = hashTree TrailingLeaf testData
        root = rootHash tree

    putStrLn $ "Is expected root hash: " ++ show (show root == expectedRootHash)
    print root

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM boolM act = boolM >>= \b -> unless b act

filePath :: FilePath
filePath = "data/test.dat"

writeTestFile :: IO ()
writeTestFile = writeFile filePath testData
  where
    numBytes = (2 ^ (22 :: Int)) + 32
    testData = replicate numBytes '\000'

expectedRootHash :: String
expectedRootHash = "e9683665a90bd70aabd7705cba57c2be2a4e913a0ca1a14d765497112c178120"
