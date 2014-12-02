import Control.Monad
import Data.IORef
import System.Exit
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

main :: IO ()
main = startGUI defaultConfig setup

width, height :: Int
width = 500
height = 400

data Direction = U | D | L | R
data Snake = Snake {
      segment :: [(Double, Double)]
    , dir :: Direction
    }

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
    foo <- string "0"
    keycode <- string "n/a"
    bdy <- getBody window
    getBody window #+ [column [
          element canvas
        , row [element start, element stop, element restart]
        , row [element foo]
        , row [element keycode]
        ]]

    drawSnake "green" canvas newSnake

    on UI.click start   . const $ UI.start timer
    on UI.click stop    . const $ UI.stop timer
    on UI.click restart . const $ do
        UI.clearCanvas canvas
        liftIO $ writeIORef t 0
        liftIO $ writeIORef snake newSnake
        drawSnake "green" canvas newSnake

    on UI.keydown bdy $ \k -> liftIO $ do
        s <- readIORef snake
        writeIORef snake $ s { dir =
            case k of
                37 -> L
                38 -> U
                39 -> R
                40 -> D
                _ -> dir s
            }

    on UI.keydown bdy $ \k -> element keycode # set text (show k)

    on UI.tick timer $ const $ do
        liftIO $ modifyIORef t (+1)
        t' <- liftIO $ readIORef t
        element foo # set text (show t')
        delTail snake canvas
        liftIO $ modifyIORef snake $ moveSnake
        snake' <- liftIO $ readIORef snake
        drawSnake "green" canvas snake'

delTail :: IORef Snake -> Element -> UI ()
delTail s canvas = do
    s' <- liftIO $ readIORef s
    let h = last $ segment s'
        snake = s' { segment = init (segment s') }
    liftIO $ writeIORef s snake
    element canvas # set UI.fillStyle (UI.htmlColor "white")
    UI.fillRect h 10 10 canvas

moveSnake :: Snake -> Snake
moveSnake s@(Snake b d) = case d of
    U -> s { segment = (x, y - tt) : b }
    D -> s { segment = (x, y + tt) : b }
    R -> s { segment = (x + tt, y) : b }
    L -> s { segment = (x - tt, y) : b }
    where
        (x, y) = head b
        tt = 10.0

drawSnake :: String -> Element -> Snake -> UI ()
drawSnake color canvas snake = do
    element canvas # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect h 10 10 canvas) (segment snake)

