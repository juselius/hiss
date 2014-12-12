module Types where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef

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
    } deriving (Show)

data Snake = Snake {
      trunk :: [(Double, Double)]
    , heading :: Move
    , stomach :: Int
    } deriving (Show)

data Food = Food {
      aisle :: (Double, Double)
    , portionSize :: Int
    , shelfLife :: Int
    } deriving (Show)

data Move = U | D | L | R deriving(Show, Eq, Ord)

