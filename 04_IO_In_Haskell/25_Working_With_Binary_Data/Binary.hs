{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

bytesExample :: B.ByteString
bytesExample = "Hello world"

bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack
