{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data World = World { intW :: Int } deriving (Show)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup _ = void $ do
    (e0, fire) <- liftIO UI.newEvent

    let e0' = action <$ e0
    e <- accumE (return (World 0)) e0'
    onEvent e (liftIO . void)
    replicateM_ 5 . liftIO $ fire ()
    where
        action :: IO World -> IO World
        action world = do
            World {..} <- world
            let w = World (succ intW)
            print w
            return w

