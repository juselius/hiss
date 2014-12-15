{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

data ActiveElements = AE {
      btn0   :: Element
    , btn1   :: Element
    , tStr0  :: Element
    , tStr1  :: Element
    }

activeElements :: UI ActiveElements
activeElements = do
    btn0  <- UI.button # set text "Start"
    btn1  <- UI.button # set text "Stop"
    tStr0 <- string "0"
    tStr1 <- string "0"
    return AE {
          btn0   = btn0
        , btn1   = btn1
        , tStr0  = tStr0
        , tStr1  = tStr1
        }

setup :: Window -> UI ()
setup window = void $ do
    void $ return window # set title "FRP Test 1"

    (AE {..}) <- activeElements
    void $ getBody window #+ [
          row [element btn0, element btn1]
        , row [string "timer = ", element tStr0]
        , row [string "ticks = ", element tStr1 ]
        ]

    tx <- UI.timer
    let eTick = (+1) <$ UI.tick tx
        eStop = const (0::Int) <$ UI.click btn1
    bTimer0 <- accumB (0::Int) ((+1) <$ eTick)
    bTimer1 <- accumB (0::Int) $ head <$> unions [eStop,  eTick]

    on UI.click btn0 $ const (UI.start tx)
    on UI.click btn1 $ const (UI.stop tx)

    onChanges bTimer0 $ \w -> runW bTimer1 w
    onChanges bTimer1 $ \w -> liftIO $ print w

    void $ element tStr1 # sink text (show <$> bTimer0)
    void $ element tStr0 # sink text (show <$> bTimer1)

runW :: Behavior Int -> Int -> UI ()
runW b t = do
    b' <- currentValue b
    liftIO $ putStr $ "t = " ++ show t
    liftIO $ putStrLn $ ", b = " ++ show b'

