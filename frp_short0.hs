import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data World = World { intW :: Int , runW :: UI () }

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup _ = void $ do
    (e0, fire) <- liftIO  UI.newEvent

    let e0' = (\w -> w { intW = succ $ intW w
        , runW = liftIO . print $ intW w }) <$ e0
    e <- accumE (World 0 (return ())) e0'
    onEvent e $ \w -> void $ runW w
    replicateM_ 5 . liftIO $ fire ()

