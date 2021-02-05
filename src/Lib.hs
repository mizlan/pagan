module Lib where

import           Control.Monad                  ( filterM )
import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )

import           System.Directory               ( listDirectory
                                                , doesFileExist
                                                , getModificationTime
                                                )

import           System.FilePath                ( (</>) )

-- |Takes in a directory and gets the most recently supported file
-- getMostRecentSupportedFile :: FilePath -> Maybe FilePath
-- getMostRecentSupportedFile path = do
-- gets files, gets most recent

-- |Gets only files (not directories) from a list of canonical FilePaths
getOnlyFiles :: [FilePath] -> IO [FilePath]
getOnlyFiles = filterM doesFileExist

-- |`listDirectory` but returns canonical paths
canonicalListDirectory :: FilePath -> IO [FilePath]
canonicalListDirectory path = do
  contents <- listDirectory path
  return (map (path </>) contents)

-- |Gets the most recent entry
getMostRecentEntry :: [FilePath] -> IO FilePath
getMostRecentEntry contents = do
  records <- mapM getModificationTime contents
  return $ fst $ maximumBy (comparing snd) $ zip contents records
