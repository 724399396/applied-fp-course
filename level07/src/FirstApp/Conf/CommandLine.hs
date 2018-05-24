module FirstApp.Conf.CommandLine
  ( commandLineParser
  ) where

import Data.Monoid (Last (Last), (<>))
import FirstApp.Types (PartialConf (..), Port (..), DBFilePath (..))
import Options.Applicative (strOption, option, optional, eitherReader, Parser, fullDesc, progDesc, header, execParser, info, helper, long ,short, metavar, help)
import Text.Read (readEither)

commandLineParser :: IO PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"

  in
    execParser $ info (helper <*> partialConfParser) mods

partialConfParser :: Parser PartialConf
partialConfParser =
  PartialConf <$> portParser <*> dbFilePathParser

portParser :: Parser (Last Port)
portParser = let
  mods = long "port"
         <> short 'p'
         <> metavar "PORT"
         <> help "TCP Port to accept requests on"
  portReader = eitherReader (fmap Port . readEither)
  in
    Last <$> optional (option portReader mods)

dbFilePathParser :: Parser (Last DBFilePath)
dbFilePathParser = let
  mods = long "db-filepath"
         <> short 'd'
         <> metavar "DBFILEPATH"
         <> help "File path for our SQLite Database file."
  in
    Last <$> optional (DBFilePath <$> strOption mods)
