module Grid where

import Data.Vector hiding (foldr)
import Data.Maybe (fromMaybe)
import Graphics.Gloss hiding (Vector, Point)
import Data.Function ((&))
import Config
import System.Random
import System.IO.Unsafe
import Control.Monad (liftM)

-- Our Grid data structure is actually just a Vector of Vectors.
-- This isn't *hugely* efficient but it's better than nested lists.

data Grid = Grid (Vector (Vector Cell))
    deriving (Show, Eq)


data Cell = Empty
          | Snake
          | Pellet
    deriving (Show, Eq)


-- Helpers for dealing with grids.
-- This way, we don't have to get our hands dirty with vectors.

-- A grid which is entirely Empty cells.
emptyGrid = Grid
    $ fromList [
        fromList [Empty | x <- [0..gridW-1]
    ] | y <- [0..gridH-1] ]


-- Grid size in tiles.
gridW :: Int
gridW = 9
gridH :: Int
gridH = 9


-- How many pixels per cell?
xUnit :: Float
xUnit = fromIntegral gridWidth  / fromIntegral gridW
yUnit :: Float
yUnit = fromIntegral gridHeight / fromIntegral gridH


-- The middle point of the grid.
midGrid :: Point
midGrid = ((gridW - 1) `div` 2,(gridH - 1) `div` 2)


-- Get the value of a cell safely, from a point.
-- Will return Nothing if out of range.
getCell :: Grid -> Point -> Maybe Cell
getCell (Grid rowVec) (x,y) = do
        row <- rowVec !? y
        row !? x


-- Set the value of a cell.
-- This is an *unsafe* operation and will crash if fed
-- out of range positions.
setCell :: Cell -> Point -> Grid -> Grid
setCell cell (x,y) (Grid rowVec) = Grid $ rowVec
        // [(y,(rowVec!y) // [(x,cell)])]


-- Get us a random unoccupied cell on the board.
-- If the space it chose was occupied, it runs the function again.
-- Don't do this!
randomEmptySquare :: Grid -> Point
randomEmptySquare grid = unsafePerformIO $ do
    -- We are using the global RNG so that we don't have to track
    -- or generate random seeds.
    rX <- liftM (`mod` gridW) $ randomIO :: IO Int
    rY <- liftM (`mod` gridH) $ randomIO :: IO Int

    let cell = getCell grid (rX, rY)
    case cell of
        Nothing    -> error "Random out of bounds?"
        Just Empty -> return (rX,rY)
        _          -> return $ randomEmptySquare grid



-- Populate an empty grid with snake and food.
-- This (with getCell) is an easier way of checking for collisions
-- than doing it by hand.
makeGrid :: [Point] -> [Point] -> Grid
makeGrid snakeCells pelletCells =
    emptyGrid
    & flip (foldr $ setCell Snake ) snakeCells
    & flip (foldr $ setCell Pellet) pelletCells



-- Create a grid and then render it to a picture.
-- Uses the logic of makeGrid internally.
renderGrid ::[Point] -> [Point] -> Picture
renderGrid snakeCells pelletCells = pictures $
    [renderCell x y | x <- [0..gridW-1], y <- [0..gridH-1]]
    where
        grid  = makeGrid snakeCells pelletCells

        renderCell x y = let
                -- Get the value of each cell.
                cell = fromMaybe
                    (error "Tried to render off-grid?")
                    (getCell grid (x,y))

                -- Relative midpoint of cell
                xRel = fromIntegral x - (fromIntegral gridW - 1) / 2
                yRel = fromIntegral y - (fromIntegral gridH - 1) / 2

                -- Position the cell based on relative distance from middle
                xPos = xRel * xUnit
                yPos = yRel * (-yUnit) -- We want to render top to bottom

                -- Cell pixel size.
                rectW = xUnit - fromIntegral marginSize
                rectH = yUnit - fromIntegral marginSize

                -- Internal rectangle "hole" for empty cells.
                emptyW = rectW * (1-emptyThickness)
                emptyH = rectH * (1-emptyThickness)

            in translate xPos yPos $ case cell of
                -- Empty cell = just a border
                Empty   -> pictures [
                            color mdColor $ rectangleSolid rectW rectH,
                            color bgColor $ rectangleSolid emptyW emptyH]

                -- Snake = bright solid rect
                Snake   -> color fgColor $ rectangleSolid rectW rectH

                -- Pellet = dim solid rect
                Pellet  -> color mdColor $ rectangleSolid rectW rectH

