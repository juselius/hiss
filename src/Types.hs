module Types where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data Game = Game {
      canvas  :: Element
    , playB   :: Element
    , scoreF  :: Element
    , highF   :: Element
    , timer   :: UI.Timer
    }

data Snake = Snake {
      trunk :: [(Double, Double)]
    , heading :: Move
    , stomach :: Int
    , corner :: (Move, Move)
    } deriving (Show)

data Food = Food {
      aisle :: (Double, Double)
    , portionSize :: Int
    , shelfLife :: Int
    } deriving (Show, Eq)

data Move = U | D | L | R deriving(Show, Eq, Ord)

