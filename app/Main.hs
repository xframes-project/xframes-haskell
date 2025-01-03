{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS

-- Individual font definition
data FontDef = FontDef
  { name :: String
  , size :: Int
  } deriving (Show, Generic)

instance ToJSON FontDef

-- Overall structure with "defs"
data FontDefs = FontDefs
  { defs :: [FontDef]
  } deriving (Show, Generic)

instance ToJSON FontDefs

fontDefs :: FontDefs
fontDefs = FontDefs $ concatMap (\(name, sizes) -> map (\size -> FontDef name size) sizes) fonts
  where
    fonts = [("roboto-regular", [16, 18, 20, 24, 28, 32, 36, 48])]

jsonData :: String
jsonData = BS.unpack $ encode fontDefs

main :: IO ()
main = putStrLn jsonData