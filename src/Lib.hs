{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified PlaySound2
import System.Directory

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  putStrLn "TEST"
  path <- System.Directory.getCurrentDirectory
  putStrLn path
  PlaySound2.withAudio $ do
    PlaySound2.playFile "./dundundun.ogg"
  -- PlaySound2.waitUntilSoundsFinished

