repeat n = cycle [n]

subseq start stop seq = drop start (take stop seq)

inFirstHalf x seq = x `elem` take (length seq `div` 2) seq
