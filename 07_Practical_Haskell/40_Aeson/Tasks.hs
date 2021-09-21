{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )

data IntList = EmptyList | Cons Int IntList deriving (Show,Generic)

instance ToJSON IntList
instance FromJSON IntList
