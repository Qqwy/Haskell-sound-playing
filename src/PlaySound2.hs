{-# LANGUAGE FlexibleContexts           #-}
module PlaySound2
    (Music,
     initAudio,
     closeAudio,
     withAudio,
     loadMusic,
     playAudio,
     playAudioLoop,
     whilePlayingAudio,
     waitUntilSoundsFinished,
     playFile) where

import qualified SDL.Mixer
import qualified SDL
import qualified Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Debug.Trace

type Music = SDL.Mixer.Music

{-| Initializes audio, but 'closeAudio' needs to be called manually.  -}
initAudio :: MonadIO m => m ()
initAudio =
  let
    audio = SDL.Mixer.defaultAudio
    -- audio = SDL.Mixer.Audio
    --       { SDL.Mixer.audioFrequency = 44100
    --       , SDL.Mixer.audioFormat = SDL.Mixer.FormatS16_Sys
    --       , SDL.Mixer.audioOutput = SDL.Mixer.Stereo }
  in SDL.Mixer.openAudio audio 4096

{-| Call after having finished playing music, when initialized using `initAudio`.-}
closeAudio :: MonadIO m => m ()
closeAudio = SDL.Mixer.closeAudio

{-| Automatically initializes audio mixer, and closes it once playback has finished. -}
withAudio :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withAudio actions = do
  let audio = SDL.Mixer.defaultAudio
  SDL.Mixer.withAudio audio 4096 actions

{-| Loads a given music file to a `SDL.Mixer.Music` type that can be played. -}
loadMusic :: MonadIO m => String -> m Music
loadMusic = SDL.Mixer.load

{-| Plays a given `SDL.Mixer.Music` instance once (in the background, program continues).-}
playAudio ::  MonadIO m => Music -> m ()
playAudio m = do
    SDL.Mixer.setMusicVolume 50
    SDL.Mixer.playMusic 1 m

{-| Plays a given `SDL.Mixer.Music` instance indefinitely (in the background, program continues).-}
playAudioLoop :: MonadIO m => Music -> m ()
playAudioLoop m = do
    SDL.Mixer.setMusicVolume 50
    SDL.Mixer.playMusic SDL.Mixer.Forever m

{-| Performs loading/freeing an audio file around the given actions. --}
whilePlayingAudio :: MonadIO m => String -> m a -> m a
whilePlayingAudio file actions = do
  m <- loadMusic file
  playAudio m
  result <- actions
  SDL.Mixer.free m
  return result

{-| Plays the given file, and blocks the program from continuing until playback has finished. -}
playFile :: (MonadIO m) => String -> m ()
playFile filename =
  whilePlayingAudio filename waitUntilSoundsFinished

{-| Blocks the program until audio playback has finished. -}
waitUntilSoundsFinished :: MonadIO m => m ()
waitUntilSoundsFinished = do
    SDL.delay 50
    stillPlaying <- SDL.Mixer.playingMusic
    Control.Monad.when stillPlaying waitUntilSoundsFinished
