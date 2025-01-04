{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Text (pack, Text)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson.Types
import Foreign
import Foreign.C
-- import Foreign.C.String
import Control.Monad
-- import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr ()
import Control.Concurrent (forkIO, threadDelay)

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
  deriving (Show, Eq, Ord, Generic, Enum)

instance ToJSON ImGuiCol where
  toJSON col = Number (fromIntegral $ fromEnum col)

instance ToJSONKey ImGuiCol where
  toJSONKey = toJSONKeyText (pack . show . fromEnum)

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

-- Define the color theme
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

theme2Json :: String
theme2Json = BS.unpack $ encode theme2

foreign import ccall "wrapper"
  wrapOnInitCb :: (IO ()) -> IO (FunPtr (IO ()))
foreign import ccall "wrapper"
  wrapOnTextChangedCb :: (CInt -> CString -> IO ()) -> IO (FunPtr (CInt -> CString -> IO ()))
foreign import ccall "wrapper"
  wrapOnComboChangedCb :: (CInt -> CInt -> IO ()) -> IO (FunPtr (CInt -> CInt -> IO ()))
foreign import ccall "wrapper"
  wrapOnNumericValueChangedCb :: (CInt -> CFloat -> IO ()) -> IO (FunPtr (CInt -> CFloat -> IO ()))
foreign import ccall "wrapper"
  wrapOnBooleanValueChangedCb :: (CInt -> CBool -> IO ()) -> IO (FunPtr (CInt -> CBool -> IO ()))
foreign import ccall "wrapper"
  wrapOnMultipleNumericValuesChangedCb :: (CInt -> Ptr CFloat -> CInt -> IO ()) -> IO (FunPtr (CInt -> Ptr CFloat -> CInt -> IO ()))
foreign import ccall "wrapper"
  wrapOnClickCb :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))


type OnInitCb = FunPtr (IO ())
type OnTextChangedCb = FunPtr (CInt -> CString -> IO ())
type OnComboChangedCb = FunPtr (CInt -> CInt -> IO ())
type OnNumericValueChangedCb = FunPtr (CInt -> CFloat -> IO ())
type OnBooleanValueChangedCb = FunPtr (CInt -> CBool -> IO ())
type OnMultipleNumericValuesChangedCb = FunPtr (CInt -> Ptr CFloat -> CInt -> IO ())
type OnClickCb = FunPtr (CInt -> IO ())

foreign import ccall "init" 
    c_init :: CString       
           -> CString        
           -> CString        
           -> OnInitCb      
           -> OnTextChangedCb  
           -> OnComboChangedCb 
           -> OnNumericValueChangedCb  
           -> OnBooleanValueChangedCb  
           -> OnMultipleNumericValuesChangedCb
           -> OnClickCb       
           -> IO ()

onInit :: IO ()
onInit = putStrLn "Initialized"

onTextChanged :: CInt -> CString -> IO ()
onTextChanged _ _ = putStrLn "Text Changed"

onComboChanged :: CInt -> CInt -> IO ()
onComboChanged _ _ = putStrLn "Combo Changed"

onNumericValueChanged :: CInt -> CFloat -> IO ()
onNumericValueChanged _ _ = putStrLn "Numeric Value Changed"

onBooleanValueChanged :: CInt -> CBool -> IO ()
onBooleanValueChanged _ _ = putStrLn "Boolean Value Changed"

onMultipleNumericValuesChanged :: CInt -> Ptr CFloat -> CInt -> IO ()
onMultipleNumericValuesChanged _ _ _ = putStrLn "Multiple Numeric Values Changed"

onClick :: CInt -> IO ()
onClick _ = putStrLn "Clicked"

infiniteLoop :: IO ()
infiniteLoop = do
  putStrLn "Press CTRL+C to terminate"
  let loop = do
        threadDelay 100000  -- Sleep for 100 ms
        loop
  loop

main :: IO ()
main = do
    assetsBasePath <- newCString "./assets"
    rawFontDefs <- newCString fontDefsJson
    rawStyleDefs <- newCString theme2Json

    onInitPtr <- wrapOnInitCb onInit
    onTextChangedPtr <- wrapOnTextChangedCb onTextChanged
    onComboChangedPtr <- wrapOnComboChangedCb onComboChanged
    onNumericValueChangedPtr <- wrapOnNumericValueChangedCb onNumericValueChanged
    onBooleanValueChangedPtr <- wrapOnBooleanValueChangedCb onBooleanValueChanged
    onMultipleNumericValuesChangedPtr <- wrapOnMultipleNumericValuesChangedCb onMultipleNumericValuesChanged
    onClickPtr <- wrapOnClickCb onClick

    c_init assetsBasePath rawFontDefs rawStyleDefs
                  onInitPtr
                  onTextChangedPtr
                  onComboChangedPtr
                  onNumericValueChangedPtr
                  onBooleanValueChangedPtr
                  onMultipleNumericValuesChangedPtr
                  onClickPtr

    forever (threadDelay 1000000)

    -- https://wiki.haskell.org/GHC/Using_the_FFI
    freeHaskellFunPtr onInitPtr
    freeHaskellFunPtr onTextChangedPtr
    freeHaskellFunPtr onComboChangedPtr
    freeHaskellFunPtr onNumericValueChangedPtr
    freeHaskellFunPtr onBooleanValueChangedPtr
    freeHaskellFunPtr onMultipleNumericValuesChangedPtr
    freeHaskellFunPtr onClickPtr
