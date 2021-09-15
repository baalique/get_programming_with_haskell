module Main where

import qualified Data.ByteString               as BC
import qualified Glitch                        as G
import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched  <- G.combineRandom 2 imageFile
    let glitchedFileName = "glitched_" ++ fileName
    BC.writeFile glitchedFileName glitched
    putStrLn "Done"
