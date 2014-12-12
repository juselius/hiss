{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data World = World { intW :: Int , runW :: UI () }

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup _ = do
    (e0, fire) <- liftIO  UI.newEvent
    let e0' = action <$ e0
    e <- accumE (World 0 (return ())) e0'
    onEvent e $ \w -> void $ runW w
    replicateM_ 5 . liftIO $ fire ()
    where
        action :: World -> World
        action w@(World {..}) = w { intW = succ intW
                     , runW = liftIO . print $ intW
                     }

