{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import Data.Maybe (maybe, fromJust)
import Data.List (partition, find)
import Data.IORef
import System.Random (randomRIO)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Sneaky

import Debug.Trace

type Gamer = UI Game -> UI Game

main :: IO ()
main = startGUI defaultConfig { tpStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = void $ do
    w <- return window # set title "Sneaky snake"
    UI.addStyleSheet w "sneakysnake.css"
    game@(Game {..}) <- createLayout w

    bdy <- getBody w
    (eNewFood, fNewFood) <- liftIO UI.newEvent
    (eEat, fEat) <- liftIO UI.newEvent
    let eTick   = (+1) <$ UI.tick timer
        eReset  = const (0::Int) <$ UI.click playB
        ePause  = UI.click pauseB
        eKey    = keycode <$> UI.keydown bdy
        eTimer  = head <$> unions [eTick, eReset]
        eSnakeT = head <$> unions [
              moveSnake <$ eTick
            , const (newSnake 5) <$ eReset
            , steerSnake <$> eKey
            , feedSnake <$> eEat
            ]
        eFoodT = head <$> unions [
              updateFood <$ eTick
            , const noFood <$ eReset
            , (:) <$> eNewFood
            , expireFood <$> eEat
            ]

    bTimer <- accumB (0::Int) eTimer
    bScore <- accumB (0::Int) $ head <$> unions [
          (+) . portionSize <$> eEat
        , eReset
        ]
    bFood <- accumB noFood eFoodT
    bSnake <- accumB (newSnake 5) eSnakeT

    element scoreF # sink text (show <$> bScore)
    element scoreF # sink value (show <$> bScore)
    onEvent eReset . const $ do wipeCanvas; UI.start timer
    onEvent ePause . const $ UI.stop timer
    onChanges bTimer $ tickActions game bSnake
    onChanges bTimer . const . liftIO $ genFood bSnake bFood fNewFood
    onChanges bTimer . const $ snakeActions game bFood fEat bSnake
    onChanges bFood  $ drawFood game

uiElements :: UI Game
uiElements =  do
    canvas <- UI.canvas
        # set UI.id_ "canvas"
        # set UI.height height
        # set UI.width width
        # set style [("border", "solid black 5px")]
        # set UI.textFont "24px sans-serif"
    playB <- UI.button #. "button" # set text "New game"
    pauseB <- UI.button #. "button" # set text "Pause"
    scoreF <- string "0"# set value "0"
    highF <- string "0" # set UI.id_ "highscore" # set value "0"
    timeF <- string "0"
    timer <- UI.timer # set UI.interval 1000
    return Game {
          canvas  = canvas
        , playB   = playB
        , pauseB  = pauseB
        , timeF   = timeF
        , scoreF  = scoreF
        , highF   = highF
        , timer   = timer
       }

createLayout :: Window -> UI Game
createLayout window = do
    elm@(Game {..}) <- uiElements
    void $ getBody window #. "wrap" #+ [column [
          greet
        , element canvas
        , row [element playB, element pauseB
            , UI.pre # set text " SCORE: ", element scoreF
            , UI.pre # set text " HIGHSCORE: ", element highF]
        , row [string "t = ", element timeF]
        ]]
    wipeCanvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor "red")
    UI.fillText "PRESS PLAY TO BEGIN" (100.0, 200.0) canvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor bgColor)
    return elm

tickActions :: Game -> Behavior Snake -> Int -> UI ()
tickActions g bs t = do
    let Game {..} = g
    score <- getScore g
    when (mod score 10 == 0) . void $
        return timer # set UI.interval (newint score)
    void $ element timeF # set text (show t)
    void $ element scoreF # set text (show score)
    snake <- currentValue bs
    drawSnake "brown" snake
    where
        newint score = truncate
            (100.0 - (5.0 * fromIntegral score / 10.0) :: Double)

snakeActions :: Game -> Behavior [Food] -> Handler Food -> Behavior Snake
    -> UI ()
snakeActions g bf fEat bSnake = do
    snake <- currentValue bSnake
    let Snake {..} = snake
    food <- currentValue bf
    let snack = find (\x -> aisle x == head trunk) food
    maybe (when (offside snake) $ gameOver g) (liftIO . fEat) snack

genFood :: Behavior Snake -> Behavior [Food] -> Handler Food -> IO ()
genFood bs bf fNewFood = do
    snake <- currentValue bs
    food <- currentValue bf
    dice <- randomRIO (1,10) :: IO Int
    when (dice == 1 && length food < 5) $ do
        f <- newFood
        unless (aisle f `elem` trunk snake) $ fNewFood f
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

drawFood :: Game -> [Food] -> UI ()
drawFood g food = do
    let Game {..} = g
    mapM_ draw food
    mapM_ clearFood food
    where
        c = canvas g
        draw x = do
            let sc = case portionSize x of
                    1 -> "blue"
                    2 -> "green"
                    3 -> "red"
                    _ -> bgColor
            void $ element c # set UI.fillStyle (UI.htmlColor sc)
            UI.fillRect (shiftX $ aisle x) m m c
        clearFood x = when (shelfLife x < 1) $ do
                void $ element c # set UI.fillStyle (UI.htmlColor bgColor)
                UI.fillRect (shiftX $ aisle x) m m c
        m = fromIntegral marker - 2.0

getScore :: Game -> UI Int
getScore g = get value (scoreF g) >>= \x -> return (read x :: Int)

resetGame :: Game -> Behavior Snake -> UI ()
resetGame g s = do
    let Game {..} = g
    void $ return timer # set UI.interval 100
    wipeCanvas
    snake <- currentValue s
    drawSnake "brown" snake
    UI.start timer

drawSnake :: String -> Snake -> UI ()
drawSnake color s = do
    let Snake {..} = s
    c <- getCanvas
    wipeTail s
    void $ element c # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect (shiftX h) m m c) (init trunk)
    where
        m = fromIntegral marker - 2.0

gameOver :: Game -> UI ()
gameOver g = void $ do
    let Game {..} = g

    UI.stop timer
    wipeCanvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor "red")
    UI.fillText "GAME OVER" (165.0, 200.0) canvas
    Just hse <- liftIO (getWindow canvas) >>= \w ->
        getElementById w "highscore"
    score <- getScore g
    hscore <- get value hse >>= \x -> return (read x :: Int)
    let nh = newHigh score hscore
    void $ element hse # set text nh
    element hse # set text nh # set value nh
    where
        newHigh cs hs = if cs > hs
            then show cs
            else show hs

wipeTail :: Snake -> UI ()
wipeTail s = do
    canvas <- getCanvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor bgColor)
    UI.fillRect (shiftX h) m m canvas
    where
        Snake {..} = s
        h = last trunk
        m = fromIntegral marker - 2.0

greet :: UI Element
greet = UI.h1  #+ [string "Haskell Interactive Strangler Snake Simulator"]

getCanvas :: UI Element
getCanvas = do
    w <- askWindow
    fromJust <$> getElementById w "canvas"

wipeCanvas :: UI ()
wipeCanvas = do
    canvas <- getCanvas
    UI.clearCanvas canvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor bgColor)
    UI.fillRect (0.0, 0.0) width' height' canvas
