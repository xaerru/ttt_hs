{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Brick.Widgets.Table
import Control.Monad (void)
import Control.Lens
import Brick.Widgets.Center (center)
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import qualified Brick.Main as M
import qualified Brick.Types as T

newtype AppState =
    AppState { _tttTable :: [[String]]
             }

makeLenses ''AppState

initialState :: AppState
initialState =
    AppState { _tttTable = replicate 3 $ replicate 3 "#"
             }

uiTable :: AppState -> Table ()
uiTable s = f [0..2] tb
    where
        f (x:xs) t = f xs $ alignCenter x t
        f [] t = t
        tb = table $ ((padTopBottom 2 . padLeftRight 3) <$>) <$> map (map str) (view tttTable s) 

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = 
        case e of
            V.EvKey V.KEsc [] -> M.halt
            V.EvKey (V.KChar '1') [] -> assign tttTable $ replicate 3 (replicate 3 "0")

            _ -> M.continueWithoutRedraw

appEvent _ = return ()

ui :: AppState -> Widget ()
ui s = center $ renderTable $ uiTable s

drawUI :: AppState -> [Widget ()]
drawUI s = [ui s]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue) ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
