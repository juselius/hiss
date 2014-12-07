import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Tasty test"

    btn0 <- UI.button # set text "Click Event"
    btn1 <- UI.button # set text "Click Behavior"
    btn2 <- UI.button # set text "Click?"

    _ <- getBody window #+ [row [
        element btn0 ,element btn1, element btn2]]

    eClick0 <- accumE (0::Int) $ (+1) <$ UI.click btn0
    onEvent eClick0 $ \x -> element btn0 # set text (show x)

    let eClick1 = subtract 1 <$ UI.click btn1
    bClick1 <- accumB (1::Int) eClick1
    element btn1 # sink text (show <$> bClick1)

    let eClick2 = bClick1 <@ eClick1
    onEvent eClick2 $ \x -> do
        element btn2 # set text (show $ -x)
        if x == -11
            then element btn0 # set text "tjosan"
            else element btn0

    onChanges bClick1 $ \x -> if (x `mod` 5) == 0
        then element btn2 # set text "prosit"
        else element btn2
