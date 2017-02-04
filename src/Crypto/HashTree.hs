module Crypto.HashTree
    ( HashTree
    , HashStrategy(..)
    , hashTree
    , rootHash
    ) where

import qualified Crypto.Hash     as Hash
import qualified Data.Byteable   as B
import qualified Data.ByteString as BS

type Hash = Hash.Digest Hash.SHA256

data HashTree = Node Hash HashTree HashTree | Leaf Hash deriving Show

data HashStrategy = TrailingLeaf | DuplicateSegment

-- Note: hash tree constructed with extra leaves at end of tree.
-- Consider adding different implementation types as specified by some enum.
--      ┌───┴──┐       ┌────┴───┐         ┌─────┴─────┐
--   ┌──┴──┐   │    ┌──┴──┐     │      ┌──┴──┐     ┌──┴──┐
-- ┌─┴─┐ ┌─┴─┐ │  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐   │
--    (5-leaf)         (6-leaf)             (7-leaf)

rootHash :: HashTree -> Hash
rootHash (Node hash _ _) = hash
rootHash (Leaf hash)     = hash

hashTree :: HashStrategy -> BS.ByteString -> HashTree
hashTree strategy bytes = case strategy of
    TrailingLeaf     -> buildTreeTrailingLeaf leaves
    DuplicateSegment -> buildTreeDuplicateSegment leaves
  where
    leaves :: [HashTree]
    leaves = Leaf . Hash.hash <$> blocks
    blocks = segments blockSize bytes
    blockSize = 64


buildTreeTrailingLeaf :: [HashTree] -> HashTree
buildTreeTrailingLeaf [x] = x
buildTreeTrailingLeaf xs  =
    joinHashTree (buildTreeTrailingLeaf left) (buildTreeTrailingLeaf right)
  where
    (left, right) = splitAt i xs
    i = until (\x -> x * 2 >= length xs) (*2) 1


buildTreeDuplicateSegment :: [HashTree] -> HashTree
buildTreeDuplicateSegment [x]    = joinHashTree x x
buildTreeDuplicateSegment [x, y] = joinHashTree x y
buildTreeDuplicateSegment xs  =
    joinHashTree (buildTreeDuplicateSegment left) (buildTreeDuplicateSegment right)
  where
    (left, right) = splitAt i xs
    i = until (\x -> x * 2 >= length xs) (*2) 1

joinHashTree :: HashTree -> HashTree -> HashTree
joinHashTree h1 h2 = Node (joinHash (rootHash h1) (rootHash h2)) h1 h2

joinHash :: Hash -> Hash -> Hash
joinHash h1 h2 = Hash.hash (BS.append (B.toBytes h1) (B.toBytes h2))

segments :: Int -> BS.ByteString -> [BS.ByteString]
segments _ xs | BS.null xs = []
segments size xs           = y : segments size ys
  where
    (y, ys) = BS.splitAt size xs
