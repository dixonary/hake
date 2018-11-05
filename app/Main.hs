{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..), SpecialKey (..) )
import qualified Graphics.Gloss.Interface.IO.Interact as K (KeyState (..))
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss as G (text)
import Graphics.Gloss hiding (text, Point)

import Prelude hiding (Either, Left, Right)
import Data.Fixed (mod')

import Grid
import Config
import Sound

{-
 - Useful base data types.
 - The World type is handed through most other functions.
 - Changes to the game state are represented as updates to the World instance.
 -}

data World = World {
    state      :: State,        -- Is the game running?
    titleFlash :: Float,        -- Flashing text timer
    dir        :: Direction,    -- Next move direction
    lastDir    :: Direction,    -- Last move direction
    snake      :: [Point],      -- Snake positions
    pellets    :: [Point],      -- Food positions
    tickPeriod :: Float,        -- Time between world updates
    tickTimer  :: Float,        -- World update timer
    sound      :: SoundInfo,    -- Stateful sound system info
    score      :: Int           -- Current score
}

data State = Title | Play | GameOver
    deriving (Eq, Show)

data Direction = Up | Down | Left | Right | None
    deriving (Eq, Show)


{-
 - The default World structure is created at the start of the game.
 - It also replaces the World when restarting after a Game Over.
 -}

defWorld       = World {
    state      = Title,               -- Start on the title screen
    titleFlash = 0,
    dir        = None,                -- No initial direction
    lastDir    = None,                -- No initial direction
    snake      = replicate startingLength midGrid,
    pellets    = [randomEmptySquare emptyGrid],
    tickPeriod = 0.3,                 -- About 3 ticks per second
    tickTimer  = tickPeriod defWorld, -- Give this a jump-start!
    sound      = initialiseSound,     -- See Sound.hs
    score      = 0
}


-- FUNCTION DEFINITIONS
-- ====================

{- The application's entry point.
 - This simply calls Gloss's `play` function which handles window management etc.
 -}

main :: IO ()
main = play
    (InWindow "Snake &&&" gameSize gamePos) -- Window information
    bgColor                                 -- Background color
    60                                      -- Max FPS
    defWorld                                -- Initial "world"
    render                                  -- Rendering function
    handleInput                             -- Input handler
    update                                  -- Update handler



{-
 - The Rendering function takes the World representation
 - and generates a Picture (a gloss renderable representation).
 - We create multiple Pictures in a list, and combine
 - them with the "pictures" function.
 -}

render :: World -> Picture
render world@World{..} = pictures $
    renderGrid snake pellets       -- Always render the grid
    :
    case state of
        -- Title screen: We only need render the title text
        Title ->
            if titleFlash `mod'` 1.5 < 0.9 then
                [translate (-290) (-350) $ text fgColor 30 1 "PRESS W/A/S/D TO START"]
            else
                []

        -- Play: Render the current score
        Play ->
            [translate (-350) (320) $ text mdColor 50 3 $ show score]

        -- Game over: Render the score, and game over text (with outlines)
        GameOver ->
            (translate (-350) (320) $ text fgColor 50 3 $ show score)
            : if titleFlash `mod'` 1.5 < 0.9 then
                [translate (-200) (0)   $ text bgColor 50 5 "GAME OVER",
                 translate (-200) (0)   $ text fgColor 50 2 "GAME OVER",
                 translate (-200) (-80) $ text bgColor 30 3 "SPACE TO RESTART",
                 translate (-200) (-80) $ text fgColor 30 1 "SPACE TO RESTART"
                 ]
            else []



{-
 - Input handler
 - This function is called automatically whenever the user interacts
 - with the game in some way.
 -
 - We don't bother handling resize events.
 -}

handleInput :: Event -> World -> World

-- Pattern match to handle character key press (wasd)
handleInput (EventKey (Char c) K.Down _ _) world  =
    let world' = world { dir =
        case c of
            'w' | lastDir world /= Down  -> Up
            's' | lastDir world /= Up    -> Down
            'a' | lastDir world /= Right -> Left
            'd' | lastDir world /= Left  -> Right
            _   -> dir world
        }
    in case state world of
        -- Only change state if the direction has been set!
        Title               -> world' { state = if dir world' == None
                                                then Title
                                                else Play
                                      }
        _                   -> world'

-- Pattern match to handle Spacebar press
handleInput (EventKey (SpecialKey KeySpace) K.Down _ _) world@World{..}  =
    case state of
        GameOver -> defWorld
            -- Play the restart sound effect.
            where !_ = playSound sound Restart
        _        -> world

-- Ignore any other inputs
handleInput e world = world



{-
 - Update is called once per "frame", ie 60 times per second.
 - We use this to time our "tick" function and do other minor updates.
 -}

update :: Float -> World -> World
update time world@World{..} = case state of

    Title -> world {
        -- Just update the flash timer.
        titleFlash = titleFlash + time
        }

    Play -> let
        -- Update the tick time, and run 'tick' as well if it's due.
            newTickTime = tickTimer + time
            newWorld = world {tickTimer = newTickTime `mod'` tickPeriod}
        in
        if newTickTime > tickPeriod then tick newWorld else newWorld

    GameOver -> world {
        -- Just update the flash timer.
        titleFlash = titleFlash + time
        }



{-
 - Tick is called less frequently and represents an update to the world state.
 - It only runs while the game is in the Play state.
 -}

tick :: World -> World
tick world@World{..} = let

    -- Play the "blip" sound of the snake moving.
    !_ = playSound sound Blip


    -- Move the snake's head one space based on current direction.
    nextPos :: Point -> Point
    nextPos (px,py) = case dir of
        Up    -> (px  ,py-1)
        Down  -> (px  ,py+1)
        Left  -> (px-1,  py)
        Right -> (px+1,  py)
        None  -> (px, py)

    -- Move the whole snake one space.
    updateSnake sn = updateSnake' sn Nothing []
        where

            {- This function is defined recursively:
             -   First parameter is the remainder of snake to update
             -   Second parameter is the piece of snake in front (if present)
             -   Third parameter is the updated snake position
             -}

            -- Start of snake
            updateSnake' (c:cs) Nothing     new =
                updateSnake' cs (Just c) (new ++ [nextPos c])

            -- Main section
            updateSnake' (c:cs) (Just prev) new =
                updateSnake' cs (Just c) (new ++ [prev])

            -- We have run out of snake!
            updateSnake' []     _           new = new

    -- Add one segment to the very end of the snake.
    extend [s]    = [s, s]
    extend (s:ss) = s : extend ss

    newSnake     = updateSnake snake
    oldGrid      = makeGrid snake pellets
    newGrid      = makeGrid newSnake pellets
    snakeHeadPos = getCell oldGrid $ head newSnake

    -- Helper for the game over state & sound effect
    gameOver = world {
        state = GameOver,
        titleFlash = 0
        } where !_ = playSound sound Die

    in
        case snakeHeadPos of
            -- Out of bounds
            Nothing -> gameOver

            -- Self collision
            Just Snake  -> gameOver

            -- Yum yum
            Just Pellet -> world { lastDir = dir,
                                   snake   = extend newSnake,
                                   pellets = [randomEmptySquare newGrid],
                                   tickPeriod = tickPeriod * 0.96,
                                   score = score + 1 }
                where !_ = playSound sound Feed

            -- Empty space
            Just Empty  -> world { lastDir = dir, snake = newSnake }



-- Helper function for rendering nicer text.
-- Yes, this does render a LOT of text items.
text :: Color -> Float -> Float -> String -> Picture
text col size boldness str =
    color col $
    pictures [translate x y $ scale (size/100) (size/100) $ G.text str
         | x <- [(-boldness),(0.5-boldness)..boldness]
         , y <- [(-boldness),(0.5-boldness)..boldness] ]
