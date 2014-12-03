import Data.Maybe
import Control.Monad
import Data.IORef
import System.Exit
import System.Random (randomRIO)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import Debug.Trace

data Move = U | D | L | R

data Snake = Snake {
      trunk :: [(Double, Double)]
    , heading :: Move
    , stomach :: Int
    }

data Food = Food {
      aisle :: (Double, Double)
    , portionSize :: Int
    , shelfLife :: Int
    } deriving (Show)

main :: IO ()
main = startGUI defaultConfig setup

width, height, marker :: Int
width = 500
height = 400
marker = 20

newSnake :: Snake
newSnake = Snake [(fromIntegral $ marker * x, 100.0) | x <- [5,4..1]] R 0

noFood :: [Food]
noFood = []

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Snakeu"
    canvas <- UI.canvas
        # set UI.height height
        # set UI.width width
        # set style [("border", "solid blue 3px")]
        # set UI.textFont "24px sans-serif"
    start <- UI.button # set text "Start"
    stop <- UI.button # set text "Stop"
    curtime <- string "0"
    timeLabel <- string "t = "
    curscore <- string "0"
    scoreLabel <- string "SCORE: "
    getBody window #+ [column [
          element canvas
        , row [element start, element stop]
        , row [element scoreLabel, element curscore]
        , row [element timeLabel, element curtime]
        ]]

    score <- liftIO $ newIORef (0::Int)
    t <- liftIO $ newIORef (0::Int)
    snake <- liftIO $ newIORef newSnake
    food <- liftIO $ newIORef noFood

    timer <- UI.timer # set UI.interval 100
    on UI.click start   . const $ do
        UI.clearCanvas canvas
        liftIO $ writeIORef t 0
        liftIO $ writeIORef snake newSnake
        drawSnake "green" canvas snake
        UI.start timer
    on UI.click stop . const $ UI.stop timer

    bdy <- getBody window
    on UI.keydown bdy $ \k -> do
        running <- get UI.running timer
        when (running && isMove k) $ do
            liftIO $ setHeading snake k
            updateSnake canvas t snake
            validateSnake canvas timer snake

    on UI.tick timer $ const $ do
        updateFood canvas food
        feedSnake snake food score
        drawFood canvas food
        updateSnake canvas t snake
        s <- validateSnake canvas timer snake
        t' <- liftIO $ readIORef t
        score' <- liftIO $ readIORef score
        element curtime # set text (show t')
        element curscore # set text (show score')

feedSnake :: IORef Snake -> IORef [Food] -> IORef Int -> UI ()
feedSnake snake food score = liftIO $ do
    f <- readIORef food
    s <- readIORef snake
    let h = head $ trunk s
        f' = filter (\(Food x _ _) -> x == h) f
        f'' = filter (\(Food x _ _) -> x /= h) f
        amount = sum $ map portionSize f'
    unless (null f') $ do
        writeIORef snake $ s { stomach =
            stomach s + amount  }
        writeIORef food f''
        modifyIORef score (+ amount)


updateSnake :: Element -> IORef Int -> IORef Snake -> UI ()
updateSnake canvas time snake = do
    liftIO $ modifyIORef time (+1)
    moveSnake canvas snake
    liftIO $ modifyIORef snake digestFood
    where
        digestFood x =  if stomach x > 0
            then x { stomach = pred $ stomach x}
            else x

updateFood :: Element -> IORef [Food] -> UI ()
updateFood canvas food = liftIO $ do
    modifyIORef food cropFood
    dice <- randomRIO (1,10) :: IO Int
    f <- readIORef food
    when (dice == 1 && length f < 5) $ do
        f' <- newFood
        modifyIORef food (f':)
    where
        newFood = do
            psize <- randomRIO (1,5)
            slife <- randomRIO (50,150)
            x <- randomRIO (0, 50) :: IO Int
            y <- randomRIO (0, 40) :: IO Int
            let x' = fromIntegral $ x * marker
                y' = fromIntegral $ y * marker
            return $ Food (x', y') psize slife
        cropFood f =
                filter (\x -> shelfLife x >= 0) $ decLife f
            where
                decLife = map (\x -> x { shelfLife = pred $ shelfLife x })

validateSnake :: Element -> UI.Timer -> IORef Snake -> UI ()
validateSnake canvas timer snake = do
    s <- liftIO $ readIORef snake
    when (offside s) $ gameOver canvas timer
    where
        offside :: Snake -> Bool
        offside s'
            | x < 0.0 || x > fromIntegral width = True
            | y < 0.0 || y > fromIntegral height = True
            | a `elem` b = True
            | otherwise = False
            where
                a@(x, y) = head $ trunk s'
                b = tail $ trunk s'

gameOver :: Element -> UI.Timer -> UI ()
gameOver canvas timer = do
    UI.clearCanvas canvas
    UI.stop timer
    UI.fillText "GAME OVER" (175.0, 200.0) canvas

moveSnake :: Element -> IORef Snake -> UI ()
moveSnake canvas snake = do
    incSnake
    s <- liftIO $ readIORef snake
    when (stomach s < 1) $
        delTail snake canvas
    drawSnake "green" canvas snake
    where
        incSnake = liftIO $ do
            s' <- readIORef snake
            let s@(Snake b d _) = s'
                (x, y) = head b
            writeIORef snake $
                case d of
                    U -> s { trunk = (x, y - tt) : b }
                    D -> s { trunk = (x, y + tt) : b }
                    R -> s { trunk = (x + tt, y) : b }
                    L -> s { trunk = (x - tt, y) : b }
                    where
                        tt = fromIntegral marker

delTail :: IORef Snake -> Element -> UI ()
delTail s canvas = do
    s' <- liftIO $ readIORef s
    let h = last $ trunk s'
        snake = s' { trunk = init (trunk s') }
    liftIO $ writeIORef s snake
    element canvas # set UI.fillStyle (UI.htmlColor "white")
    UI.fillRect h m m canvas
    where
        m = fromIntegral marker

drawSnake :: String -> Element -> IORef Snake -> UI ()
drawSnake color canvas snake = do
    s <- liftIO $ readIORef snake
    element canvas # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect h m m canvas) (trunk s)
    where
        m = fromIntegral marker

drawFood :: Element -> IORef [Food] -> UI ()
drawFood canvas food = do
    f <- liftIO $ readIORef food
    mapM_ draw f
    mapM_ clearFood f
    where
        draw x = do
            let c = case portionSize x of
                    1 -> "yellow"
                    2 -> "red"
                    3 -> "green"
                    _ -> "white"
            element canvas # set UI.fillStyle (UI.htmlColor c)
            UI.fillRect (aisle x) m m canvas
        clearFood x = when (shelfLife x < 1) $ do
                element canvas # set UI.fillStyle (UI.htmlColor "white")
                UI.fillRect (aisle x) m m canvas
        m = fromIntegral marker


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

