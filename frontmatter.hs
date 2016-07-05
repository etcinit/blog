#!/usr/bin/env stack
-- stack --install-ghc runghc --package uuid --package time

module Main where

import Control.Monad (mapM_)
import Data.Monoid ((<>))
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)

main :: IO ()
main = do
  uuid <- nextRandom

  time <- getCurrentTime

  let formattedTime = formatTime defaultTimeLocale "%c" time

  mapM_ putStrLn $
    [ "---"
    , "title: CHANGEME"
    , "date: " <> formattedTime
    , "author: Eduardo Trujillo"
    , "uuid: " <> toString uuid
    , "---"
    ]
