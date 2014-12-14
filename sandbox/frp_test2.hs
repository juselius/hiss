{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

data World = World {
      strW :: String
    , timeW :: Int
    }

data ActiveElements = AE {
      btn0   :: Element
    , btn1   :: Element
    , btn2   :: Element
    , infos  :: Element
    , tStr0  :: Element
    , tStr1  :: Element
    }

activeElements :: UI ActiveElements
activeElements = do
    btn0  <- UI.button # set text "Start"
    btn1  <- UI.button # set text "Stop"
    btn2  <- UI.button # set text "More!"
    infos <- string "waiting..."
    tStr0 <- string "0"
    tStr1 <- string "0"
    return AE {
          btn0   = btn0
        , btn1   = btn1
        , btn2   = btn2
        , infos  = infos
        , tStr0  = tStr0
        , tStr1  = tStr1
        }

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "FRP Test 1"
    (AE {..}) <- activeElements
    let wrl = World "" 0

    _ <- getBody window #+ [
          row [element btn0, element btn1, element btn2]
        , row [string "stat = ", element infos]
        , row [string "timer = ", element tStr0]
        , row [string "ticks = ", element tStr1 ]
        ]
    tx <- UI.timer
    let eTick = UI.tick tx
    on UI.click btn0 $ const (UI.start tx)
    on UI.click btn1 $ const (UI.stop tx)
    let bStop = pure (0) <@ UI.click btn1
    bTimer <- accumB (0::Int) ((+1) <$ eTick)
    eTimer <- accumE (0::Int) ((+1) <$ eTick)
    let eTimer' = bStop <@ eTimer
    bTimer' <- stepper (0::Int) eTimer'
    element tStr1 # sink text (show <$> bTimer)
    element tStr0 # sink text (show <$> bTimer')
    onChanges bTimer $ \w -> runW w
    --onChanges bStop $ \w -> liftIO $ print (w 1)


runW :: Int -> UI ()
runW t = liftIO $ putStrLn $ "t = " ++ show t

