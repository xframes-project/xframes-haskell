{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson.Key (fromString)

data ImGuiCol
  = Text
  | TextDisabled
  | WindowBg
  | ChildBg
  | PopupBg
  | Border
  | BorderShadow
  | FrameBg
  | FrameBgHovered
  | FrameBgActive
  | TitleBg
  | TitleBgActive
  | TitleBgCollapsed
  | MenuBarBg
  | ScrollbarBg
  | ScrollbarGrab
  | ScrollbarGrabHovered
  | ScrollbarGrabActive
  | CheckMark
  | SliderGrab
  | SliderGrabActive
  | Button
  | ButtonHovered
  | ButtonActive
  | Header
  | HeaderHovered
  | HeaderActive
  | Separator
  | SeparatorHovered
  | SeparatorActive
  | ResizeGrip
  | ResizeGripHovered
  | ResizeGripActive
  | Tab
  | TabHovered
  | TabActive
  | TabUnfocused
  | TabUnfocusedActive
  | PlotLines
  | PlotLinesHovered
  | PlotHistogram
  | PlotHistogramHovered
  | TableHeaderBg
  | TableBorderStrong
  | TableBorderLight
  | TableRowBg
  | TableRowBgAlt
  | TextSelectedBg
  | DragDropTarget
  | NavHighlight
  | NavWindowingHighlight
  | NavWindowingDimBg
  | ModalWindowDimBg
  | COUNT
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ImGuiCol where
  toJSON Text = Number 0
  toJSON TextDisabled = Number 1
  toJSON WindowBg = Number 2
  toJSON ChildBg = Number 3
  toJSON PopupBg = Number 4
  toJSON Border = Number 5
  toJSON BorderShadow = Number 6
  toJSON FrameBg = Number 7
  toJSON FrameBgHovered = Number 8
  toJSON FrameBgActive = Number 9
  toJSON TitleBg = Number 10
  toJSON TitleBgActive = Number 11
  toJSON TitleBgCollapsed = Number 12
  toJSON MenuBarBg = Number 13
  toJSON ScrollbarBg = Number 14
  toJSON ScrollbarGrab = Number 15
  toJSON ScrollbarGrabHovered = Number 16
  toJSON ScrollbarGrabActive = Number 17
  toJSON CheckMark = Number 18
  toJSON SliderGrab = Number 19
  toJSON SliderGrabActive = Number 20
  toJSON Button = Number 21
  toJSON ButtonHovered = Number 22
  toJSON ButtonActive = Number 23
  toJSON Header = Number 24
  toJSON HeaderHovered = Number 25
  toJSON HeaderActive = Number 26
  toJSON Separator = Number 27
  toJSON SeparatorHovered = Number 28
  toJSON SeparatorActive = Number 29
  toJSON ResizeGrip = Number 30
  toJSON ResizeGripHovered = Number 31
  toJSON ResizeGripActive = Number 32
  toJSON Tab = Number 33
  toJSON TabHovered = Number 34
  toJSON TabActive = Number 35
  toJSON TabUnfocused = Number 36
  toJSON TabUnfocusedActive = Number 37
  toJSON PlotLines = Number 38
  toJSON PlotLinesHovered = Number 39
  toJSON PlotHistogram = Number 40
  toJSON PlotHistogramHovered = Number 41
  toJSON TableHeaderBg = Number 42
  toJSON TableBorderStrong = Number 43
  toJSON TableBorderLight = Number 44
  toJSON TableRowBg = Number 45
  toJSON TableRowBgAlt = Number 46
  toJSON TextSelectedBg = Number 47
  toJSON DragDropTarget = Number 48
  toJSON NavHighlight = Number 49
  toJSON NavWindowingHighlight = Number 50
  toJSON NavWindowingDimBg = Number 51
  toJSON ModalWindowDimBg = Number 52
  toJSON COUNT = Number 53

-- Define the color constants
theme2Colors :: Map.Map Text Text
theme2Colors = Map.fromList
  [ ("darkestGrey", "#141f2c")
  , ("darkerGrey", "#2a2e39")
  , ("darkGrey", "#363b4a")
  , ("lightGrey", "#5a5a5a")
  , ("lighterGrey", "#7A818C")
  , ("evenLighterGrey", "#8491a3")
  , ("black", "#0A0B0D")
  , ("green", "#75f986")
  , ("red", "#ff0062")
  , ("white", "#fff")
  ]

theme2 :: Map.Map ImGuiCol (Text, Int)
theme2 = Map.fromList
  [ (Text, (theme2Colors Map.! "white", 1))
  , (TextDisabled, (theme2Colors Map.! "lighterGrey", 1))
  , (WindowBg, (theme2Colors Map.! "black", 1))
  , (ChildBg, (theme2Colors Map.! "black", 1))
  , (PopupBg, (theme2Colors Map.! "white", 1))
  , (Border, (theme2Colors Map.! "lightGrey", 1))
  , (BorderShadow, (theme2Colors Map.! "darkestGrey", 1))
  , (FrameBg, (theme2Colors Map.! "black", 1))
  , (FrameBgHovered, (theme2Colors Map.! "darkerGrey", 1))
  , (FrameBgActive, (theme2Colors Map.! "lightGrey", 1))
  , (TitleBg, (theme2Colors Map.! "lightGrey", 1))
  , (TitleBgActive, (theme2Colors Map.! "darkerGrey", 1))
  , (TitleBgCollapsed, (theme2Colors Map.! "lightGrey", 1))
  , (MenuBarBg, (theme2Colors Map.! "lightGrey", 1))
  , (ScrollbarBg, (theme2Colors Map.! "darkerGrey", 1))
  , (ScrollbarGrab, (theme2Colors Map.! "darkerGrey", 1))
  , (ScrollbarGrabHovered, (theme2Colors Map.! "lightGrey", 1))
  , (ScrollbarGrabActive, (theme2Colors Map.! "darkestGrey", 1))
  , (CheckMark, (theme2Colors Map.! "darkestGrey", 1))
  , (SliderGrab, (theme2Colors Map.! "darkerGrey", 1))
  , (SliderGrabActive, (theme2Colors Map.! "lightGrey", 1))
  , (Button, (theme2Colors Map.! "black", 1))
  , (ButtonHovered, (theme2Colors Map.! "darkerGrey", 1))
  , (ButtonActive, (theme2Colors Map.! "black", 1))
  , (Header, (theme2Colors Map.! "black", 1))
  , (HeaderHovered, (theme2Colors Map.! "black", 1))
  , (HeaderActive, (theme2Colors Map.! "lightGrey", 1))
  , (Separator, (theme2Colors Map.! "darkestGrey", 1))
  , (SeparatorHovered, (theme2Colors Map.! "lightGrey", 1))
  , (SeparatorActive, (theme2Colors Map.! "lightGrey", 1))
  , (ResizeGrip, (theme2Colors Map.! "black", 1))
  , (ResizeGripHovered, (theme2Colors Map.! "lightGrey", 1))
  , (ResizeGripActive, (theme2Colors Map.! "darkerGrey", 1))
  , (Tab, (theme2Colors Map.! "black", 1))
  , (TabHovered, (theme2Colors Map.! "darkerGrey", 1))
  , (TabActive, (theme2Colors Map.! "lightGrey", 1))
  , (TabUnfocused, (theme2Colors Map.! "black", 1))
  , (TabUnfocusedActive, (theme2Colors Map.! "lightGrey", 1))
  , (PlotLines, (theme2Colors Map.! "darkerGrey", 1))
  , (PlotLinesHovered, (theme2Colors Map.! "lightGrey", 1))
  , (PlotHistogram, (theme2Colors Map.! "darkerGrey", 1))
  , (PlotHistogramHovered, (theme2Colors Map.! "lightGrey", 1))
  , (TableHeaderBg, (theme2Colors Map.! "black", 1))
  , (TableBorderStrong, (theme2Colors Map.! "lightGrey", 1))
  , (TableBorderLight, (theme2Colors Map.! "darkerGrey", 1))
  , (TableRowBg, (theme2Colors Map.! "darkGrey", 1))
  , (TableRowBgAlt, (theme2Colors Map.! "darkerGrey", 1))
  , (TextSelectedBg, (theme2Colors Map.! "darkerGrey", 1))
  , (DragDropTarget, (theme2Colors Map.! "darkerGrey", 1))
  , (NavHighlight, (theme2Colors Map.! "darkerGrey", 1))
  , (NavWindowingHighlight, (theme2Colors Map.! "darkerGrey", 1))
  , (NavWindowingDimBg, (theme2Colors Map.! "darkerGrey", 1))
  , (ModalWindowDimBg, (theme2Colors Map.! "darkerGrey", 1))
  ]



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

fontDefsJson :: String
fontDefsJson = BS.unpack $ encode fontDefs

-- theme2Json :: String
-- theme2Json = BS.unpack $ encode theme2

main :: IO ()
main = do
  putStrLn fontDefsJson
  -- putStrLn theme2Json