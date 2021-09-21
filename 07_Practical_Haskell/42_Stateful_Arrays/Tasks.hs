import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Data.Array.ST                  ( readArray
                                                , runSTUArray
                                                , thaw
                                                , writeArray
                                                )
import           Data.Array.Unboxed             ( (!)
                                                , IArray(bounds)
                                                , UArray
                                                , listArray
                                                )

testArr1 :: UArray Int Int
testArr1 = listArray (0, 5) [1, 1, 1, 0, 0, 0]

testArr2 :: UArray Int Int
testArr2 = listArray (0, 5) [0, 0, 0, 1, 1, 1]

crossing :: UArray Int Int -> UArray Int Int -> Int -> UArray Int Int
crossing arr1 arr2 point = runSTUArray $ do
    stArr <- thaw arr1
    let end = (snd . bounds) arr1
    forM_ [point .. end] $ \i -> do
        writeArray stArr i $ arr2 ! i
    return stArr

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros arr = runSTUArray $ do
    stArr <- thaw arr
    let end = (snd . bounds) arr
    forM_ [0 .. end] $ \i -> do
        val <- readArray stArr i
        when (val == 0) $ do
            writeArray stArr i (-1)
    return stArr
