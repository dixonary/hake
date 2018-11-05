{-# LANGUAGE RecordWildCards #-}

module Sound where

import Sound.ALUT
import System.IO.Unsafe
import Data.Maybe (maybeToList)

-- Our Sound data structure keeps track of stateful
-- sound system information.

data SoundInfo = SoundInfo {
    device       :: Device,
    context      :: Context,
    soundSources :: [(SoundFile, Source)]
}


-- Each of the sound files is represented in this enum.
-- We can use this to populate soundSources.

data SoundFile =
    Blip
  | Die
  | Feed
  | Restart
    deriving (Bounded, Enum, Eq)


-- Where in filesystem the sound files are,
-- relative to project root.
soundPath :: SoundFile -> String
soundPath Blip     = "assets/blip.wav"
soundPath Die      = "assets/die.wav"
soundPath Feed     = "assets/pellet.wav"
soundPath Restart  = "assets/wizzle.wav"


-- Run from Main.hs
-- Use ALUT to prepare, load our sound files,
-- and initialise the default sound device.
initialiseSound :: SoundInfo
initialiseSound = unsafePerformIO $
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context

    let
        -- Load our sound file enum into an array.
        soundFiles :: [SoundFile]
        soundFiles = [minBound .. maxBound]

        -- Generate buffer queue for each sound.
        loadBuffer sf = do
            buf <- createBuffer $ File $ soundPath sf
            [src] <- genObjectNames 1
            queueBuffers src [buf]
            return (sf, src)

    -- Run loadBuffer for each soundFile.
    sounds <- mapM loadBuffer soundFiles

    -- Construct our stateful SoundInfo.
    return $ SoundInfo device context sounds



-- (Unsafely) play a sound.
-- In Main.hs this is run outside of the IO context
-- with bang-patterns inside of let- or where-blocks.

playSound :: SoundInfo -> SoundFile -> ()
playSound (SoundInfo{..}) s = unsafePerformIO $
    withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
    do
        currentContext $= Just context
        play $ maybeToList $ lookup s soundSources
        return ()
