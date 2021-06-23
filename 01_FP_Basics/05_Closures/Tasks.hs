ifEven func x = if even x then func x else x

ifEvenInc = ifEven (+ 1)

ifEvenDouble = ifEven (* 2)

ifEvenSquare = ifEven (flip (^) 2)

--

binaryPartialApplication func x = \y -> func x y
