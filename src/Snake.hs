{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import Data.Maybe (fromJust)
import Data.List (partition)
import Data.IORef
import System.Random (randomRIO)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Sneaky

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
    (eScore, fScore) <- liftIO UI.newEvent
    let eTick   = (+1) <$ UI.tick timer
        eReset  = const (0::Int) <$ UI.click playB
        ePause  = UI.click pauseB
        eKey    = keycode <$> UI.keydown bdy
        eTimer  = head <$> unions [eTick, eReset]
        eSnakeT = head <$> unions [
              moveSnake <$ eTick
            , const (newSnake 5) <$ eReset
            , steerSnake <$> eKey
            ]
        eFoodT = head <$> unions [
              updateFood <$ eTick
            , const noFood <$ eReset
            , (:) <$> eNewFood
            , deleteNth <$> eScore
            ]

    bTimer <- accumB (0::Int) eTimer
    bScore <- accumB (0::Int) $ head <$> unions [(+1) <$ eScore, eReset]
    bFood <- accumB noFood eFoodT
    bSnake <- accumB (newSnake 5) $
        (.) <$> (pure feedSnake <*> bFood) <@> eSnakeT

    onEvent eReset . const $ do
        wipeCanvas
        UI.start timer
    onEvent ePause . const $ UI.stop timer
    onChanges bTimer $ tickActions game bSnake bFood
    onChanges bSnake $ snakeActions game bFood
    onChanges bFood  $ foodActions  game bSnake

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

tickActions :: Game -> Behavior Snake -> Behavior [Food] -> Int -> UI ()
tickActions g bs bf t = do
    let Game {..} = g
    liftIO . print =<< get value scoreF
    score <- get value scoreF >>= \s -> return (read s)
    when (mod score 10 == 0) . void $
        return timer # set UI.interval (newint score)
    void $ element timeF # set text (show t)
    void $ element scoreF # set text (show score)
    snake <- currentValue bs
    drawSnake "brown" snake
    where
        newint score = truncate
            (100.0 - (5.0 * fromIntegral score / 10.0) :: Double)

snakeActions :: Game -> Behavior [Food] -> Snake -> UI ()
snakeActions g bf snake = return ()
    --bdy <- getBody w
    --on UI.keydown bdy $ \k -> do
        --running <- get UI.running timer'
        --snake' <- getCurrentSnake g
        --when (running && isMove k) $ do
            --st <- liftIO . readIORef $ state g
            --liftIO $ writeIORef (state g) (st { snake = setHeading k snake' })
            --updateSnake g
            --st' <- liftIO . readIORef $ state g
            --when (offside (snake st')) $ gameOver g
            --liftIO . writeIORef (state g) $ feedSnake st'
    --where
        --timer' = timer g

foodActions :: Game -> Behavior Snake -> [Food] -> UI ()
foodActions g bs food = return ()

resetGame :: Game -> Behavior Snake -> UI ()
resetGame g s = do
    let Game {..} = g
    void $ return timer # set UI.interval 100
    wipeCanvas
    snake <- currentValue s
    drawSnake "brown" snake
    UI.start timer

{-setupButtonActions :: Game -> UI ()-}
{-setupButtonActions g = do-}
    {-on UI.click (startBtn g) . const $ do-}
        {-void $ return (timer g) # set UI.interval 100-}
        {-wipeCanvas canvas'-}
        {-liftIO $ writeIORef (state g) newGameState-}
        {-drawSnake "brown" g-}
        {-UI.start (timer g)-}
    {-on UI.click (stopBtn g). const $ UI.stop (timer g)-}
    {-where-}
        {-canvas' = canvas g-}

{-updateSnake :: Snake -> UI Snake-}
{-updateSnake s = do-}
    {-let s' = digestFood $ moveSnake s-}
    {-c <- getCanvas-}
    {-when (stomach s' < 1) $-}
        {-wipeTail c s    -- note: the original snake-}
    {-return s'-}
    {-where-}
        {-digestFood x =  if stomach x > 0-}
            {-then x { stomach = pred $ stomach x}-}
            {-else x-}

drawSnake :: String -> Snake -> UI ()
drawSnake color s = do
    let Snake {..} = s
    c <- getCanvas
    wipeTail s
    void $ element c # set UI.fillStyle (UI.htmlColor color)
    mapM_ (\h -> UI.fillRect h m m c) (init trunk)
    where
        m = fromIntegral marker

{-updateFood :: Game -> UI ()-}
{-updateFood game = do-}
    {-s <- getCurrentSnake game-}
    {-liftIO $ do-}
        {-st <- readIORef $ state game-}
        {-let food' = cropFood (food st)-}
        {-dice <- randomRIO (1,10) :: IO Int-}
        {-if dice == 1 && length food' < 5-}
        {-then do-}
            {-f <- newFood-}
            {-unless (aisle f `elem` trunk s) $-}
                {-writeIORef (state game) $ st { food = f:food' }-}
        {-else-}
            {-writeIORef (state game) $ st { food = food' }-}
        {-where-}
            {-newFood = do-}
                {-psize <- randomRIO (1,5)-}
                {-slife <- randomRIO (50,150)-}
                {-x <- randomRIO (0, 25) :: IO Int-}
                {-y <- randomRIO (0, 20) :: IO Int-}
                {-let x' = fromIntegral $ x * marker-}
                    {-y' = fromIntegral $ y * marker-}
                {-return $ Food (x', y') psize slife-}
            {-cropFood f =-}
                    {-filter (\x -> shelfLife x >= 0) $ decLife f-}
                {-where-}
                    {-decLife = map (\x -> x { shelfLife = pred $ shelfLife x })-}

{-gameOver :: Game -> UI ()-}
{-gameOver g = void $ do-}
    {-UI.stop t-}
    {-wipeCanvas c-}
    {-void $ element c # set UI.fillStyle (UI.htmlColor "red")-}
    {-UI.fillText "GAME OVER" (165.0, 200.0) c-}
    {-cscore <- (liftIO . readIORef) (state g) >>= \s -> return $ score s-}
    {-Just hse <- liftIO (getWindow (canvas g)) >>= \w ->-}
        {-getElementById w "highscore"-}
    {-hscore <- get value hse-}
    {-let nh = newHigh cscore hscore-}
    {-void $ element hse # set text nh-}
    {-element hse # set text nh # set value nh-}
    {-where-}
        {-newHigh cs hs = if cs > hs'-}
            {-then show cs-}
            {-else show hs'-}
            {-where-}
                {-hs' = read hs :: Int-}
        {-c = canvas g-}
        {-t = timer g-}

wipeTail :: Snake -> UI ()
wipeTail s = do
    canvas <- getCanvas
    void $ element canvas # set UI.fillStyle (UI.htmlColor bgColor)
    UI.fillRect h m m canvas
    where
        Snake {..} = s
        h = last trunk
        m = fromIntegral marker

--feedSnake :: Game -> UI Game
--feedSnake g@(Game {..}) = do
    --food' <- UI.currentValue food
    --snake'@(Snake {..}) <- UI.currentValue snake
    --let (f', f'') = partition (\(Food x _ _) -> x == h) food'
        --h = head $ trunk
        --amount = sum $ map portionSize f'
    --if null f'
    --then return g
    --else return g {
          --snake = snake' { stomach = stomach + amount  } <$ snake
        --, food = f'' <$ food
        --, score = (+ amount) <$> score
        --}


{-drawFood :: Game -> UI ()-}
{-drawFood g = do-}
    {-fud <- liftIO . readIORef $ state g-}
    {-let f = food fud-}
    {-mapM_ draw f-}
    {-mapM_ clearFood f-}
    {-where-}
        {-c = canvas g-}
        {-draw x = do-}
            {-let sc = case portionSize x of-}
                    {-1 -> "blue"-}
                    {-2 -> "green"-}
                    {-3 -> "red"-}
                    {-_ -> bgColor-}
            {-void $ element c # set UI.fillStyle (UI.htmlColor sc)-}
            {-UI.fillRect (aisle x) m m c-}
        {-clearFood x = when (shelfLife x < 1) $ do-}
                {-void $ element c # set UI.fillStyle (UI.htmlColor bgColor)-}
                {-UI.fillRect (aisle x) m m c-}
        {-m = fromIntegral marker-}


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
