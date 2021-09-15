module Glitch
  ( combineRandom
  , randomSortSection
  , randomReplaceBytes
  ) where

import           Control.Monad                  ( foldM )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           System.Environment             ( getArgs )
import           System.Random                  ( randomRIO )

intToChar :: Int -> Char
intToChar = toEnum . (`mod` 255)

intToBC :: Int -> BC.ByteString
intToBC = BC.pack . (: []) . intToChar

replaceBytes :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceBytes idx val bytes = mconcat [before, newChar, after]
 where
  (before, rest) = BC.splitAt idx bytes
  after          = BC.drop 1 rest
  newChar        = intToBC val

randomReplaceBytes :: BC.ByteString -> IO BC.ByteString
randomReplaceBytes bytes = do
  let len = BC.length bytes
  idx <- randomRIO (1, len)
  val <- randomRIO (0, 255)
  return $ replaceBytes idx val bytes

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, change, after]
 where
  (before, rest ) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  change          = BC.reverse $ BC.sort target

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return $ sortSection start sectionSize bytes

combineRandom :: Int -> BC.ByteString -> IO BC.ByteString
combineRandom n img = foldM (flip id) img $ take (n * 2) $ cycle [randomReplaceBytes, randomSortSection]
