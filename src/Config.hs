module Config where

import Graphics.Gloss hiding (Point)

-- Measured in pixels.
gameSize = (800,800)
gridSize = (600,600)

-- Shorthand accessors for gameSize and gridSize.
gameWidth  = fst gameSize
gameHeight = snd gameSize

gridWidth  = fst gridSize
gridHeight = snd gridSize


-- Where to place the game on the screen, based on the game size.
-- 1920 x 1080 is my monitor resolution.
gamePos :: (Int,Int)
gamePos  = ((1920-gameWidth) `div` 2, (1080-gameHeight) `div` 2)

-- Color shorthands: Dark blue; Light blue; Green
bgColor = makeColorI 0x33 0x2c 0x50 0xff
mdColor = makeColorI 0x46 0x87 0x8f 0xff
fgColor = makeColorI 0x94 0xe3 0x44 0xff

-- How much space to leave between grid cells
marginSize = 5

-- How thick relative to the cell size, should the empty wall be?
emptyThickness :: Float
emptyThickness = 0.2

-- How long should the snake start as
startingLength :: Int
startingLength = 3

-- Helper data type used everywhere.
type Point = (Int,Int)

