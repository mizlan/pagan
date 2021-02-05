module Lib where

import           Control.Monad                  ( filterM )
import           Data.List                      ( maximumBy
                                                , find
                                                , foldl'
                                                )
import           Data.Text                     as T
                                                ( replace
                                                , pack
                                                , unpack
                                                )
import           Data.Ord                       ( comparing )

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

configEntrySupports :: Config -> FilePath -> Bool
configEntrySupports config file = getConfigLanguage config `isExtensionOf` file

-- |TODO: rewrite this function
-- |Returns if a file is supported by a list of configurations
isSupportedBy :: [Config] -> FilePath -> Bool
isSupportedBy configs file =
  any (`isExtensionOf` file) (getConfigLanguage <$> configs)

getDefaultConfigs :: [Config]
getDefaultConfigs =
  -- lang    build command (if any)                 run command
  ----------------------------------------------------------------
  [ ("hs"  , Just "ghc {FILE}"              , "./{BASE}")
  , ("cpp" , Just "g++-10 -std=c++11 {FILE}", "./a.out")
  , ("py"  , Nothing                        , "python {FILE}")
  , ("java", Just "javac {FILE}"            , "java {BASE}")
  , ("c"   , Just "gcc {FILE} -o {BASE}"    , "./{BASE}")
  , ("ml", Just "ocamlopt str.cmxa -unsafe {FILE}", "./a.out")
  , ("jl"  , Nothing                        , "julia {FILE}")
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

interpolateCommand :: String -> FilePath -> String
interpolateCommand command path =
  let replacements =
        [ ("{BASE}", takeBaseName)
        , ("{FILE}", takeFileName)
        , ("{DIR}" , takeDirectory)
        , ("{EXT}" , getPureExtension)
        ]
  in  T.unpack $ foldl'
        (\str (var, func) -> T.replace (T.pack var) (T.pack $ func path) str)
        (T.pack command)
        replacements

-- -- |Gets a configuration from a directory
-- getRelevantConfigEntry :: [Config] -> FilePath -> IO (Maybe Config)
-- getRelevantConfigEntry configs directory = do
--   file <- canonicalListDirectory directory >>= (getOnlyFiles . getSupportedFiles configs) >>= getMostRecentEntry
--   case file of
--     Nothing -> return ()
--   print file
--   return $ findConfig file configs
