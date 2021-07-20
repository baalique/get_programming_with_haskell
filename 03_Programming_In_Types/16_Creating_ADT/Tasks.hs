data Circle = Circle
    { radius :: Double
    }

data Square = Square
    { heightSquare :: Double
    }

data Rectangle = Rectangle
    { heightRectangle :: Double
    , widthRectangle  :: Double
    }

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape    c   ) = 2 * pi * radius c
perimeter (SquareShape    sq  ) = 4 * heightSquare sq
perimeter (RectangleShape rect) = 2 * heightRectangle rect + 2 * widthRectangle rect

area :: Shape -> Double
area (CircleShape    c   ) = pi * radius c ^ 2
area (SquareShape    sq  ) = heightSquare sq ^ 2
area (RectangleShape rect) = heightRectangle rect * widthRectangle rect
