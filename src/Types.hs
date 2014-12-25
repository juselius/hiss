module Types where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef

data Game = Game {
      canvas  :: Element
    , playB   :: Element
    , pauseB  :: Element
    , scoreF  :: Element
    , highF   :: Element
    , timeF   :: Element
    , timer   :: UI.Timer
    }

data Snake = Snake {
      trunk :: [(Double, Double)]
    , heading :: Move
    , stomach :: Int
    } deriving (Show)

data Food = Food {
      aisle :: (Double, Double)
    , portionSize :: Int
    , shelfLife :: Int
    } deriving (Show, Eq)

data Move = U | D | L | R deriving(Show, Eq, Ord)

