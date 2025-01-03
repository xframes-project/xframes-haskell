{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

main :: IO ()
main = do
  let person = Person "Alice" 30
  print $ encode person