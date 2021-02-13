{-# LANGUAGE LambdaCase #-}

module Lib where

import           Control.Monad                  ( filterM
                                                , (>=>)
                                                )
import           Control.Monad.Loops            ( maximumOnM )
import           Data.List                      ( find
                                                , foldl'
                                                )
import qualified Data.Text                     as T
                                                ( replace
                                                , pack
                                                , unpack
                                                )
import           Data.Foldable                  ( for_ )
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
import           System.Process                 ( readCreateProcessWithExitCode
                                                , shell
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , die
                                                )

type BuildCommand = String
type RunCommand = String
type Config = (String, Maybe BuildCommand, RunCommand)

canonicalListDirectory :: FilePath -> IO [FilePath]
canonicalListDirectory path = do
  contents <- listDirectory path
  return (map (path </>) contents)

getMostRecentEntry :: [FilePath] -> IO (Maybe FilePath)
getMostRecentEntry = maximumOnM getModificationTime

getConfigLanguage :: Config -> String
getConfigLanguage (lang, _, _) = lang

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
findConfig file =
  find (\config -> getConfigLanguage config `isExtensionOf` file)

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
        (\str (accum, extractElement) ->
          T.replace (T.pack accum) (T.pack $ extractElement path) str
        )
        (T.pack command)
        replacements

interpolateConfig :: FilePath -> Config -> Config
interpolateConfig path (lang, buildCmd, runCmd) =
  (lang, interpolateCommand path <$> buildCmd, interpolateCommand path runCmd)

getConfig :: [Config] -> FilePath -> IO (Maybe Config)
getConfig configs dir =
  interpolate
    <$> (   getMostRecentEntry
        =<< filterM doesFileExist
        .   getSupportedFiles configs
        =<< canonicalListDirectory dir
        )
 where
  interpolate =
    (>>= \path -> interpolateConfig path <$> findConfig path configs)

getConfigUsingDefault :: FilePath -> IO (Maybe Config)
getConfigUsingDefault = getConfig getDefaultConfigs

runCommand :: String -> IO (ExitCode, String, String)
runCommand cmd = readCreateProcessWithExitCode (shell cmd) ""

executeConfig :: Config -> IO ()
executeConfig (_, mbuild, run) = do
  for_ mbuild $ runCommand >=> \case
    (ExitSuccess  , stdout, _     ) -> putStrLn $ "Success" ++ stdout
    (ExitFailure _, _     , stderr) -> die $ show $ "build failure" ++ stderr
  runCommand run >>= \case
    (ExitSuccess  , stdout, _     ) -> putStrLn $ "Success" ++ stdout
    (ExitFailure _, _     , stderr) -> die $ show $ "run failure" ++ stderr
