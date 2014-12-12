import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

data World = World {
      wInt :: Int
    , wStr :: String
    , wTime :: Int
    , wTimer :: UI.Timer
    , wUI :: UI Int
    , wRun :: UI ()
    }

data ActiveElements = AE {
      btn0  :: Element
    , btn1  :: Element
    , btn2  :: Element
    , infos :: Element
    , tStr0  :: Element
    , tStr1  :: Element
    }

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "FRP Test 1"

    btn0' <- UI.button # set text "Start"
    btn1' <- UI.button # set text "Stop"
    btn2' <- UI.button # set text "Reset"
    infos' <- string "waiting..."
    tStr0' <- string "0"
    tStr1' <- string "0"

    _ <- getBody window #+ [
          row [element btn0', element btn1', element btn2']
        , row [string "stat = ", element infos']
        , row [
              string "timex = ", element tStr0'
            , string "   timer = ", element tStr1'
            ]
        ]

    tx <- UI.timer
    let ae = AE btn0' btn1' btn2' infos' tStr0' tStr1'
        wrl = World 0 " $ " 0 tx (return 0) (return ())
    runDemo ae wrl

runDemo :: ActiveElements -> World -> UI ()
runDemo ae wrl = void $ do
    let eClick0 = clickAction0 ae <$ UI.click (btn0 ae)
        eClick1 = clickAction1 ae <$ UI.click (btn1 ae)
        eClick2 = const wrl <$ UI.click (btn2 ae)
        eTimer = UI.tick (wTimer wrl)
        eTimex = (\w -> w {
                wTime = succ (wTime w)
              , wRun = return ()
            }) <$ eTimer

    bTimer <- accumB (0::Int) $ (+1) <$ eTimer
    element (tStr1 ae) # sink text (show <$> bTimer)
    eActions <- accumE wrl $ concatenate <$> unions [
          eClick0
        , eClick1
        , eClick2
        , eTimex
        ]

    onEvent eActions $ \w -> void $ wRun w

clickAction0 :: ActiveElements -> World -> World
clickAction0 ae w =
    let i = wInt w
        ws = wStr w in
    w {
      wInt = i + 1
    , wStr = ws ++ show i ++ ","
    , wRun = void $ UI.start (wTimer w) >> showState ae w
    , wUI = wUI w >>= \j -> return (j+1)
    }

clickAction1 :: ActiveElements -> World -> World
clickAction1 ae w =
    let i = wInt w
        ws = wStr w in
    w {
      wInt = i
    , wStr = ws ++ show i ++ "."
    , wRun = void $ UI.stop (wTimer w) >> showState ae w
    , wUI = wUI w >>= \j -> return (j-1)
    }

showState :: ActiveElements -> World -> UI ()
showState ae w = void $ do
    j <- wUI w
    liftIO . putStrLn $ "time=" ++ show (wTime w)
    element (infos ae) # set text (
        show (wInt w)
        ++ " @ " ++ show j
        ++ " : " ++ wStr w
        )
    element (tStr0 ae) # set text (show (wTime w))
