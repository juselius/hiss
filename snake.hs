import Control.Monad
import Data.IORef
import System.Random (randomRIO)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
--import Reactive.Threepenny

data Game = Game {
      canvas    :: Element
    , startBtn  :: Element
    , stopBtn   :: Element
    , curTime   :: Element
    , curScore  :: Element
    , timer     :: UI.Timer
    , state     :: IORef GameState
    }

data GameState = GameState {
      snake    :: Snake
    , food     :: [Food]
    , time     :: Int
    , score    :: Int
    }

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

data Move = U | D | L | R

width, height, marker :: Int
width = 500
height = 400
marker = 20

width', height' :: Double
width' = fromIntegral width
height' = fromIntegral height

main :: IO ()
main = startGUI defaultConfig { tpStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = void $ do
    w <- return window # set title "Sneaky snake"
    UI.addStyleSheet w "sneakysnake.css"
    game <- createLayout w
    setupButtonActions game
    setupKeyActions w game
    setupTimerActions game

setupTimerActions :: Game -> UI ()
setupTimerActions g = on UI.tick timer' $ const $ do
        updateSnake g
        updateFood g
        st <- liftIO $ readIORef $ state g
        let st' = feedSnake st
            snake' = snake st'
            score' = score st'
            t = time st'
            newint = truncate $
                (100.0 - (5.0 * fromIntegral score' / 10.0) :: Double)
        when (offside snake') $ gameOver g
        when (mod score' 10 == 0) . void $
            return timer' # set UI.interval newint
        _ <- element (curTime g)  # set text (show t)
        _ <- element (curScore g) # set text (show score')
        liftIO . writeIORef (state g) $ st'
        drawFood g
    where
        timer' = timer g

getCurrentSnake :: Game -> UI Snake
getCurrentSnake g = liftIO $ do
    s <- readIORef $ state g
    return $ snake s

setupKeyActions :: Window -> Game -> UI ()
setupKeyActions w g = do
    bdy <- getBody w
    on UI.keydown bdy $ \k -> do
        running <- get UI.running timer'
        snake' <- getCurrentSnake g
        when (running && isMove k) $ do
            st <- liftIO . readIORef $ state g
            liftIO $ writeIORef (state g) (st { snake = setHeading snake' k })
            updateSnake g
            st' <- liftIO . readIORef $ state g
            when (offside (snake st')) $ gameOver g
            liftIO . writeIORef (state g) $ feedSnake st'
    where
        timer' = timer g

setupButtonActions :: Game -> UI ()
setupButtonActions g = do
    on UI.click (startBtn g) . const $ do
        _ <- return (timer g) # set UI.interval 100
        UI.clearCanvas canvas'
        wipeCanvas canvas'
        liftIO $ writeIORef (state g) newGameState
        drawSnake "brown" g
        UI.start (timer g)
    on UI.click (stopBtn g). const $ UI.stop (timer g)
    where
        canvas' = canvas g

createLayout :: Window -> UI Game
createLayout window = do
    canvas' <- UI.canvas
        # set UI.height height
        # set UI.width width
        # set style [("border", "solid black 5px")]
        # set UI.textFont "24px sans-serif"
    start <- UI.button #. "button" # set text "Start"
    stop <- UI.button #. "button" # set text "Stop"
    curtime <- string "0"
    timeLabel <- string "t = "
    curscore <- string "0"
    scoreLabel <- UI.pre # set text "  SCORE: "
    _ <- getBody window #. "wrap" #+ [column [
          greet
        , element canvas'
        , row [element start, element stop
            , element scoreLabel, element curscore]
        , row [element timeLabel, element curtime]
        ]]
    wipeCanvas canvas'
    timer' <- UI.timer # set UI.interval 100
    st <- liftIO $ newIORef newGameState
    return $ Game canvas' start stop curtime curscore timer' st

wipeCanvas :: Element -> UI ()
wipeCanvas c = do
    _ <- element c # set UI.fillStyle (UI.htmlColor "white")
    UI.fillRect (0.0, 0.0) width' height' c

feedSnake :: GameState -> GameState
feedSnake st = if null f'
    then st
    else st {
          snake = s { stomach = stomach s + amount  }
        , food = f''
        , score = score st + amount
        }
    where
        f = food st
        s = snake st
        h = head $ trunk s
        f' = filter (\(Food x _ _) -> x == h) f
        f'' = filter (\(Food x _ _) -> x /= h) f
        amount = sum $ map portionSize f'

updateSnake :: Game -> UI ()
updateSnake g = do
    st <- liftIO . readIORef $ state g
    let t = time st
        s = snake st
        s' = digestFood $ moveSnake s
    liftIO $ writeIORef (state g) st {
          time = t + 1
        , snake = s'}
    drawSnake "brown" g
    when (stomach s' < 1) $
        wipeTail c s    -- note: the original snake
    where
        digestFood x =  if stomach x > 0
            then x { stomach = pred $ stomach x}
            else x
        c = canvas g

updateFood :: Game -> UI ()
updateFood game = do
    s <- getCurrentSnake game
    liftIO $ do
        st <- readIORef $ state game
        let food' = cropFood (food st)
        dice <- randomRIO (1,10) :: IO Int
        if (dice == 1 && length food' < 5)
        then do
            f <- newFood
            unless (aisle f `elem` trunk s) $
                writeIORef (state game) $ st { food = f:food' }
        else
            writeIORef (state game) $ st { food = food' }
        where
            newFood = do
                psize <- randomRIO (1,5)
                slife <- randomRIO (50,150)
                x <- randomRIO (0, 25) :: IO Int
                y <- randomRIO (0, 20) :: IO Int
                let x' = fromIntegral $ x * marker
                    y' = fromIntegral $ y * marker
                return $ Food (x', y') psize slife
            cropFood f =
                    filter (\x -> shelfLife x >= 0) $ decLife f
                where
                    decLife = map (\x -> x { shelfLife = pred $ shelfLife x })

offside :: Snake -> Bool
offside s
    | x < 0.0 || x >= fromIntegral width = True
    | y < 0.0 || y >= fromIntegral height = True
    | a `elem` b = True
    | otherwise = False
    where
        a@(x, y) = head $ trunk s
        b = tail $ trunk s

gameOver :: Game -> UI ()
gameOver g = do
    UI.stop t
    wipeCanvas c
    _ <- element c # set UI.fillStyle (UI.htmlColor "red")
    UI.fillText "GAME OVER" (165.0, 200.0) c
    where
        c = canvas g
        t = timer g

moveSnake ::  Snake -> Snake
moveSnake s@(Snake b d _) =
    s { trunk = newHead:newTail  }
    where
        newTail = if stomach s < 1
            then init $ trunk s
            else trunk s
        newHead =
            case d of
                U -> (x, y - tt)
                D -> (x, y + tt)
                R -> (x + tt, y)
                L -> (x - tt, y)
                where
                    (x, y) = head b
                    tt = fromIntegral marker

wipeTail :: Element -> Snake -> UI ()
wipeTail c s = do
    _ <- element c # set UI.fillStyle (UI.htmlColor "white")
    UI.fillRect h m m c
    where
        h = last $ trunk s
        m = fromIntegral marker

drawSnake :: String -> Game -> UI ()
drawSnake color g = do
    st <- liftIO . readIORef $ state g
    let s = snake st
    _ <- element c # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect h m m c) (trunk s)
    where
        c = canvas g
        m = fromIntegral marker

drawFood :: Game -> UI ()
drawFood g = do
    fud <- liftIO . readIORef $ state g
    let f = food fud
    mapM_ draw f
    mapM_ clearFood f
    where
        c = canvas g
        draw x = do
            let sc = case portionSize x of
                    1 -> "blue"
                    2 -> "green"
                    3 -> "red"
                    _ -> "white"
            _ <- element c # set UI.fillStyle (UI.htmlColor sc)
            UI.fillRect (aisle x) m m c
        clearFood x = when (shelfLife x < 1) $ do
                _ <- element c # set UI.fillStyle (UI.htmlColor "white")
                UI.fillRect (aisle x) m m c
        m = fromIntegral marker

isMove :: Int -> Bool
isMove k = case k of
        37 -> True
        38 -> True
        39 -> True
        40 -> True
        _ -> False

setHeading :: Snake -> Int -> Snake
setHeading s k = s {
    heading = case k of
        37 -> L
        38 -> U
        39 -> R
        40 -> D
        _ -> heading s
        }

greet :: UI Element
greet = UI.h1  #+ [string "Haskell Interactive Strangler Snake Simulator"]

newSnake :: Snake
newSnake = Snake [(fromIntegral $ marker * x, 100.0) | x <- [5,4..1]] R 0

newGameState :: GameState
newGameState = GameState newSnake noFood 0 0

noFood :: [Food]
noFood = []

