{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data World = World { intW :: Int } deriving (Show)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup _ = do
    (e0, fire) <- liftIO  UI.newEvent
    let e0' = action <$ e0
    e <- accumE (World 0) e0'
    b <- stepper (World 0) e
    onEvent e $ \w -> runW b w
    replicateM_ 5 . liftIO $ fire ()
    replicateM_ 5 . liftIO $ fire ()

action :: World -> World
action w@(World {..}) = w { intW = succ intW }

runW :: Behavior World -> World -> UI ()
runW b (World w) = do
    (World b') <- currentValue b
    liftIO $ print $ show b' ++ " <> " ++ show w
