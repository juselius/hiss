import Control.Monad
import Data.IORef
import System.Exit
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

data Move = U | D | L | R

data Snake = Snake {
      trunk :: [(Double, Double)]
    , heading :: Move
    }

main :: IO ()
main = startGUI defaultConfig setup

width, height :: Int
width = 500
height = 400

newSnake :: Snake
newSnake = Snake [(10.0 * x, 100.0) | x <- [9,8..1]] R

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Snakeu"
    timer <- UI.timer # set UI.interval 250
    canvas <- UI.canvas
        # set UI.height height
        # set UI.width width
        # set style [("border", "solid blue 3px")]
    start <- UI.button # set text "Start"
    stop <- UI.button # set text "Stop"
    restart <- UI.button # set text "Restart"
    t <- liftIO $ newIORef (0::Int)
    snake <- liftIO $ newIORef newSnake
    curtime <- string "0"
    keycode <- string "n/a"
    bdy <- getBody window
    getBody window #+ [column [
          element canvas
        , row [element start, element stop, element restart]
        , row [element curtime]
        ]]
    drawSnake "green" canvas snake

    on UI.click start   . const $ UI.start timer
    on UI.click stop    . const $ UI.stop timer
    on UI.click restart . const $ do
        UI.clearCanvas canvas
        liftIO $ writeIORef t 0
        liftIO $ writeIORef snake newSnake
        drawSnake "green" canvas snake

    on UI.keydown bdy $ \k -> do
        running <- get UI.running timer
        if running
            then do
                liftIO $ setHeading snake k
                if isMove k
                    then updateSnake canvas t snake
                    else return ()
            else return ()

    on UI.tick timer $ const $ do
        updateSnake canvas t snake
        t' <- liftIO $ readIORef t
        element curtime # set text (show t')

updateSnake :: Element -> IORef Int -> IORef Snake -> UI ()
updateSnake canvas time snake = do
        liftIO $ modifyIORef time (+1)
        t <- liftIO $ readIORef time
        moveSnake canvas snake

delTail :: IORef Snake -> Element -> UI ()
delTail s canvas = do
    s' <- liftIO $ readIORef s
    let h = last $ trunk s'
        snake = s' { trunk = init (trunk s') }
    liftIO $ writeIORef s snake
    element canvas # set UI.fillStyle (UI.htmlColor "white")
    UI.fillRect h 10 10 canvas

moveSnake :: Element -> IORef Snake -> UI ()
moveSnake canvas snake = do
    delTail snake canvas
    chSnake
    drawSnake "green" canvas snake
    where
        chSnake = liftIO $ do
            s' <- readIORef snake
            let s@(Snake b d) = s'
                (x, y) = head b
            writeIORef snake $
                case d of
                    U -> s { trunk = (x, y - tt) : b }
                    D -> s { trunk = (x, y + tt) : b }
                    R -> s { trunk = (x + tt, y) : b }
                    L -> s { trunk = (x - tt, y) : b }
                    where
                        tt = 10.0

drawSnake :: String -> Element -> IORef Snake -> UI ()
drawSnake color canvas snake = do
    s <- liftIO $ readIORef snake
    element canvas # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect h 10 10 canvas) (trunk s)

isMove :: Int -> Bool
isMove k = case k of
        37 -> True
        38 -> True
        39 -> True
        40 -> True
        _ -> False

setHeading :: IORef Snake -> Int -> IO ()
setHeading snake k = do
    s <- readIORef snake
    writeIORef snake $ s { heading = case k of
        37 -> L
        38 -> U
        39 -> R
        40 -> D
        _ -> heading s
        }

