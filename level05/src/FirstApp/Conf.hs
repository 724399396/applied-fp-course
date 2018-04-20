{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Monoid               (Last (..), (<>))

import           FirstApp.Types            (Conf(Conf), ConfigError(..),
                                            DBFilePath (DBFilePath),
                                            PartialConf (..), Port (Port))

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf =
  PartialConf (pure $ Port 3000) (pure $ DBFilePath "app_db.db")

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf <$> lastToEither MissingPort pcPort <*> lastToEither MissingDBFile pcDBFilePath
  where
    lastToEither :: ConfigError -> (PartialConf -> Last a) -> Either ConfigError a
    lastToEither e g = maybe (Left e) Right $ getLast $ g pc

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp = do
  -- Parse the options from the config file: "appconfig.json"
  fc <- parseJSONConfigFile fp
  -- Parse the options from the commandline using 'commandLineParser'
  cc <- commandLineParser
  -- Combine these with the default configuration 'defaultConf'
  return (fc >>= \c -> makeConfig (defaultConf <> cc <> c))
  -- Return the final configuration value

