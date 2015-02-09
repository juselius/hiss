{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad hiding (forM_)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Foldable (forM_)
import System.Random (randomRIO)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Sneaky

--import Debug.Trace

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
        eKey    = keymap <$> UI.keydown bdy
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

    void $ element scoreF # sink text (show <$> bScore)
    void $ element scoreF # sink value (show <$> bScore)
    onEvent eReset . const $ do wipeCanvas; UI.start timer
    onChanges bTimer . const $ tickActions game bFood fEat bSnake
    onChanges bTimer . const . liftIO $ genFood bSnake bFood fNewFood
    onChanges bSnake $ snakeActions game
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
    scoreF <- string "0"# set value "0"
    highF <- string "0" # set UI.id_ "highscore" # set value "0"
    timer <- UI.timer # set UI.interval startInterval
    return Game {
          canvas  = canvas
        , playB   = playB
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
        , row [element playB
            , UI.pre # set text " SCORE: ", element scoreF
            , UI.pre # set text " HIGHSCORE: ", element highF]
            ]
        ]
    wipeCanvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor snakeColor)
    void $ element canvas # set UI.fillStyle (UI.htmlColor "green")
    UI.fillText "PRESS NEW GAME TO BEGIN" (80.0, 200.0) canvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor bgColor)
    return elm

tickActions :: Game -> Behavior [Food] -> Handler Food -> Behavior Snake -> UI ()
tickActions g bf fEat bs = do
    let Game {..} = g
    score <- getScore g
    when (mod score 10 == 0) . void $
        return timer # set UI.interval (newint score)
    void $ element scoreF # set text (show score)
    snake <- currentValue bs
    food <- currentValue bf
    let snack = find (\x -> aisle x == head (trunk snake)) food
    forM_ snack $ liftIO . fEat
    where
        newint score = startInterval - 5 * fromIntegral score `div` 10

snakeActions :: Game -> Snake -> UI ()
snakeActions g snake = do
    let Snake {..} = snake
    when (offside snake) $ gameOver g
    drawSnake g snake

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
        --cropFood f =
                --filter (\x -> shelfLife x >= 0) $ decLife f
            --where
                --decLife = map (\x -> x { shelfLife = pred $ shelfLife x })

drawFood :: Game -> [Food] -> UI ()
drawFood g food = do
    let Game {..} = g
    mapM_ draw food
    mapM_ clearFood food
    where
        c = canvas g
        draw q = do
            let sc = case portionSize q of
                    1 -> "blue"
                    2 -> "magenta"
                    3 -> "red"
                    _ -> bgColor
            void $ element c # set UI.fillStyle (UI.htmlColor sc)
            let (x, y) = aisle q
            UI.fillRect (x + 2.0, y + 2.0) m m c
        clearFood q
            | shelfLife q < 0 = do
                --void $ element c # set UI.fillStyle (UI.htmlColor snakeColor)
                --let drawCircle = do
                    c # set' UI.fillStyle (UI.htmlColor snakeColor)
                    c # UI.beginPath
                    c # UI.arc (x + 10.0, y + 10.0) 12.0 0 (2 * pi)
                    c # UI.closePath
                    c # UI.fill
                --drawCircle
            | shelfLife q < 1 = do
                void $ element c # set UI.fillStyle (UI.htmlColor bgColor)
                UI.fillRect (aisle q) m m c
            | otherwise = return ()
            where (x, y) = aisle q
        m = fromIntegral marker - 4.0

getScore :: Game -> UI Int
getScore g = get value (scoreF g) >>= \x -> return (read x :: Int)

--resetGame :: Game -> Behavior Snake -> UI ()
--resetGame g s = do
    --let Game {..} = g
    --void $ return timer # set UI.interval startInterval
    --wipeCanvas
    --snake <- currentValue s
    --drawSnake g snake
    --UI.start timer

drawSnake :: Game -> Snake -> UI ()
drawSnake g s = do
    let Snake {..} = s
    when (stomach < 1) $ wipeTail s
    void $ element (canvas g) # set UI.fillStyle (UI.htmlColor snakeColor)
    drawSnakeElement g s

drawSnakeElement :: Game -> Snake -> UI ()
drawSnakeElement g snake = do
    let Snake {..} = snake
        p@(x, y) = head trunk
        up  = UI.fillRect (x + 2.0, y + 2.0) (m - 2.0) m c
        dn  = UI.fillRect (x + 2.0, y      ) (m - 2.0) m c
        rht = UI.fillRect (x      , y + 2.0) m (m - 2.0) c
        lft = UI.fillRect (x + 2.0, y + 2.0) m (m - 2.0) c
        ud  = UI.fillRect (x + 2.0, y) (m - 2.0) m' c
        rl  = UI.fillRect (x, y + 2.0) m' (m - 2.0) c
    void $ element (canvas g) # set UI.fillStyle (UI.htmlColor bgColor)
    UI.fillRect p m' m' c
    void $ element (canvas g) # set UI.fillStyle (UI.htmlColor snakeColor)
    case corner of
        (U, R) -> up >> lft
        (U, L) -> up >> rht
        (D, R) -> dn >> lft
        (D, L) -> dn >> rht
        (R, U) -> rht >> dn
        (R, D) -> rht >> up
        (L, U) -> lft >> dn
        (L, D) -> lft >> up
        (U, _) -> ud
        (D, _) -> ud
        (R, _) -> rl
        (L, _) -> rl
    where
        m' = fromIntegral marker
        m = m' - 2.0
        c = canvas g

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
    UI.fillRect (x - 2.0, y - 2.0) (m + 4.0) (m + 4.0) canvas
    where
        Snake {..} = s
        (x, y) = last trunk
        m = fromIntegral marker

greet :: UI Element
greet = UI.h1  #+ [string "Haskell Interactive Snake Simulator"]

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
