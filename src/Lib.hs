module Lib where

import           Control.Monad                  ( filterM )
import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )

import           System.Directory               ( listDirectory
                                                , doesFileExist
                                                , getModificationTime
                                                )

import           System.FilePath                ( (</>) )

type Config = (String, Maybe BuildCommand, RunCommand)

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

getConfigLanguage :: Config -> String
getConfigLanguage (lang, _, _) = lang

getConfigBuildCommand :: Config -> Maybe BuildCommand
getConfigBuildCommand (_, cmd, _) = cmd

getConfigRunCommand :: Config -> BuildCommand
getConfigRunCommand (_, _, cmd) = cmd


-- |Returns if a file is supported by a list of configurations
isSupportedBy :: FilePath -> [Config] -> Bool
isSupportedBy fileconfigs = any (`isExtensionOf` file) (getConfigLanguage <$> configs)

getDefaultConfig :: [Config]
getDefaultConfig =
  -- lang    build command (if any)                 run command
  ----------------------------------------------------------------
  [ ("hs"  , Just "ghc FILE"                      , "./BASE")
  , ("cpp" , Just "g++-10 -std=c++11 FILE"        , "./a.out")
  , ("py"  , Nothing                              , "python FILE")
  , ("java", Just "javac FILE"                    , "java BASE")
  , ("c"   , Just "gcc FILE -o BASE"              , "./BASE")
  , ("ml"  , Just "ocamlopt str.cmxa -unsafe FILE", "./a.out")
  , ("jl"  , Nothing                              , "julia FILE")
  ]

getSupportedFiles :: [FilePath] -> [Config] -> [FilePath]
getSupportedFiles contents = filter isSupportedBy
