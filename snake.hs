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

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Snakeu"
    timer <- UI.timer # set UI.interval 500
    canvas <- UI.canvas
        # set UI.height height
        # set UI.width width
        # set style [("border", "solid blue 3px")]
    start <- UI.button # set text "Start"
    stop <- UI.button # set text "Stop"
    restart <- UI.button # set text "Restart"
    t <- liftIO $ newIORef (0::Int)
    snake <- liftIO $ newIORef (Snake [(50.0, 60.0)] R)
    foo <- string "0"
    keycode <- string "n/a"
    bdy <- getBody window
    getBody window #+ [column [
          element canvas
        , row [element start, element stop, element restart]
        , row [element foo]
        , row [element keycode]
        ]]

    on UI.click start   . const $ UI.start timer
    on UI.click stop    . const $ UI.stop timer
    on UI.click restart . const $ UI.clearCanvas canvas

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
        snake'' <- liftIO $ readIORef snake
        drawSnake "white" canvas snake''
        liftIO $ modifyIORef snake $ moveSnake
        snake' <- liftIO $ readIORef snake
        drawSnake "green" canvas snake'

moveSnake :: Snake -> Snake
moveSnake s@(Snake b d) = case d of
    U -> s { segment = map (\(x,y) -> (x, y - tt)) b }
    D -> s { segment = map (\(x,y) -> (x, y + tt)) b }
    R -> s { segment = map (\(x,y) -> (x + tt, y)) b }
    L -> s { segment = map (\(x,y) -> (x - tt, y)) b }
    where
        tt = 10.0

drawSnake :: String -> Element -> Snake -> UI ()
drawSnake color canvas snake = do
    element canvas # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect h 10 10 canvas) (segment snake)
