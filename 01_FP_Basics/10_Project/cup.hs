-- cup ml = \_ -> ml

cup ml = \message -> message ml

aCup = cup 180
coffeeCup = cup 500

getMl aCup = aCup (\ml -> ml)

-- drink aCup mlDrank = cup (ml - mlDrank) where ml = getMl aCup

drink aCup mlDrank = if mlDiff >= 0 then cup mlDiff else cup 0
 where
  ml     = getMl aCup
  mlDiff = ml - mlDrank

isEmpty aCup = getMl aCup == 0

afterManySips = foldl drink coffeeCup [30, 30, 30, 30, 30]
