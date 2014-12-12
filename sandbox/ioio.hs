import Control.Monad
import Data.List

data World = World {intW :: Int}

main :: IO ()
main = void $ do
    let x = return $ World 0
    foldl' (\acc _ -> f acc) x $ replicate 5 ()

f :: IO World -> IO World
f x = do
    x' <- x
    print $ intW x'
    return $ x' { intW = 1 + intW x'}
