-- Copyright (c) 2016-present, Facebook, Inc. All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:
-- 
--  * Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
-- 
--  * Neither the name Facebook nor the names of its contributors may be used to
--    endorse or promote products derived from this software without specific
--    prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
-- ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Data.TimeZone
  ( loadTimeZoneSeries
  ) where

import qualified Control.Exception as E
import Control.Monad.Extra
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.String
import qualified Data.Text as Text
import Data.Time (TimeZone(..))
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import System.Directory
import System.FilePath

import Prelude

-- | Reference implementation for pulling TimeZoneSeries data from local
-- Olson files.
-- Many linux distros have Olson data in "/usr/share/zoneinfo/"
loadTimeZoneSeries :: FilePath -> IO (HashMap Text TimeZoneSeries)
loadTimeZoneSeries base = do
  files <- getFiles base
  tzSeries <- mapM parseOlsonFile files
  -- This data is large, will live a long time, and essentially be constant,
  -- so it's a perfect candidate for compact regions
  return $ HashMap.fromList $ rights tzSeries
  where
    -- Multiple versions of the data can exist. We intentionally ignore the
    -- posix and right formats
    ignored_dirs = HashSet.fromList $ map (base </>)
      [ "posix", "right" ]

    -- Recursively crawls a directory to list every file underneath it,
    -- ignoring certain directories as needed
    getFiles :: FilePath -> IO [FilePath]
    getFiles dir = do
      fsAll <- getDirectoryContents dir
      let
        fs = filter notDotFile fsAll
        full_fs = map (dir </>) fs
      (dirs, files) <- partitionM doesDirectoryExist full_fs

      subdirs <- concatMapM getFiles
        [ d | d <- dirs, not $ HashSet.member d ignored_dirs ]

      return $ files ++ subdirs

    -- Attempts to read a file in Olson format and returns its
    -- canonical name (file path relative to the base) and the data
    parseOlsonFile :: FilePath
                   -> IO (Either E.ErrorCall (Text, TimeZoneSeries))
    parseOlsonFile f = E.try $ do
      r <- getTimeZoneSeriesFromOlsonFile f
      return (Text.pack $ makeRelative base f, r)

    notDotFile s = not $ elem s [".", ".."]
