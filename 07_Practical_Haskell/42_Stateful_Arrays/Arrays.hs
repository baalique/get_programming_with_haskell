import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Control.Monad.ST               ( ST )
import           Data.Array.ST                  ( MArray(newArray)
                                                , STUArray
                                                , readArray
                                                , runSTUArray
                                                , thaw
                                                , writeArray
                                                )
import           Data.Array.Unboxed             ( (//)
                                                , IArray(bounds)
                                                , UArray
                                                , accum
                                                , array
                                                , listArray
                                                )

box :: UArray Int Int
box = array (0, 3) []

box' :: UArray Int Int
box' = array (0, 3) $ zip [0 .. 3] $ cycle [1, 2]

updatedBox :: UArray Int Int
updatedBox = box' // [(1, 5), (3, 6)]

updatedBox' :: UArray Int Int
updatedBox' = accum (*) updatedBox $ zip [0 .. 3] $ repeat 3

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    stArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray stArray i val
    return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ do
    let end = length vals - 1
    stArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray stArray i val
    return stArray

testData :: UArray Int Int
testData = listArray (0, 5) [23, 4, 15, 8, 42, 16]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort arr = runSTUArray $ do
    stArray <- thaw arr
    let end = (snd . bounds) arr
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val     <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j       nextVal
                writeArray stArray (j + 1) val
    return stArray
