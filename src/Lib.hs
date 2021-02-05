module Lib where

import           Control.Monad                  ( filterM )
import           Control.Monad.Loops            ( maximumOnM )
import           Data.List                      ( find
                                                , foldl'
                                                )
import           Data.Text                     as T
                                                ( replace
                                                , pack
                                                , unpack
                                                )
import           System.Directory               ( listDirectory
                                                , doesFileExist
                                                , getModificationTime
                                                )

import           System.FilePath                ( (</>)
                                                , isExtensionOf
                                                , takeBaseName
                                                , takeExtension
                                                , takeFileName
                                                , takeDirectory
                                                )

type BuildCommand = String
type RunCommand = String
type Config = (String, Maybe BuildCommand, RunCommand)

getOnlyFiles :: [FilePath] -> IO [FilePath]
getOnlyFiles = filterM doesFileExist

canonicalListDirectory :: FilePath -> IO [FilePath]
canonicalListDirectory path = do
  contents <- listDirectory path
  return (map (path </>) contents)

getMostRecentEntry :: [FilePath] -> IO (Maybe FilePath)
getMostRecentEntry = maximumOnM getModificationTime

getConfigLanguage :: Config -> String
getConfigLanguage (lang, _, _) = lang

getConfigBuildCommand :: Config -> Maybe BuildCommand
getConfigBuildCommand (_, cmd, _) = cmd

getConfigRunCommand :: Config -> BuildCommand
getConfigRunCommand (_, _, cmd) = cmd

configEntrySupports :: Config -> FilePath -> Bool
configEntrySupports config file = getConfigLanguage config `isExtensionOf` file

isSupportedBy :: [Config] -> FilePath -> Bool
isSupportedBy configs file =
  any (`isExtensionOf` file) (getConfigLanguage <$> configs)

getDefaultConfigs :: [Config]
getDefaultConfigs =
  -- lang  | build command (if any)         | run command --
  ----------------------------------------------------------
  [ ("hs"  , Just "ghc $FILE"              , "./$BASE")
  , ("cpp" , Just "g++-10 -std=c++11 $FILE", "./a.out")
  , ("py"  , Nothing                       , "python $FILE")
  , ("java", Just "javac $FILE"            , "java $BASE")
  , ("c"   , Just "gcc $FILE -o $BASE"     , "./$BASE")
  , ("ml"  , Just "ocamlopt str.cmxa $FILE", "./a.out")
  , ("jl"  , Nothing                       , "julia $FILE")
  ]

getSupportedFiles :: [Config] -> [FilePath] -> [FilePath]
getSupportedFiles configs = filter (isSupportedBy configs)

-- |Gets a configuration from a filename
findConfig :: FilePath -> [Config] -> Maybe Config
findConfig file = find (`configEntrySupports` file)

getPureExtension :: FilePath -> String
getPureExtension path = case takeExtension path of
  ""          -> ""
  ('.' : ext) -> ext
  _           -> path

interpolateCommand :: FilePath -> String -> String
interpolateCommand path command =
  let replacements =
        [ ("$BASE", takeBaseName)
        , ("$FILE", takeFileName)
        , ("$DIR" , takeDirectory)
        , ("$EXT" , getPureExtension)
        ]
  in  T.unpack $ foldl'
        (\str (var, func) -> T.replace (T.pack var) (T.pack $ func path) str)
        (T.pack command)
        replacements

interpolateConfig :: FilePath -> Config -> Config
interpolateConfig path (lang, buildCmd, runCmd) = (lang, interpolateCommand path <$> buildCmd, interpolateCommand path runCmd)

getConfig :: [Config] -> FilePath -> IO (Maybe Config)
getConfig configs dir =
  interpolate <$> (getMostRecentEntry =<< getOnlyFiles . getSupportedFiles configs =<< canonicalListDirectory dir)
  where interpolate = (>>= \path -> interpolateConfig path <$> findConfig path configs)
