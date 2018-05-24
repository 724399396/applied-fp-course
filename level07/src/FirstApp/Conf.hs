module FirstApp.Conf where

import FirstApp.Types (PartialConf (..), Port (..), DBFilePath (..), ConfigError (..), Conf (..))
import Data.Monoid (Last (..))
import FirstApp.Conf.File (parseJSONConfigFile)
import FirstApp.Conf.CommandLine ( commandLineParser )
import Data.Semigroup ((<>))

defaultConf :: PartialConf
defaultConf = PartialConf (Last (Just (Port 3000))) (Last (Just (DBFilePath "app_db.db")))

makeConfig :: PartialConf -> Either ConfigError Conf
makeConfig (PartialConf p db) = case getLast p of
  Nothing -> Left $ MissingConfig "missing port number"
  Just p' -> case getLast db of
    Nothing -> Left $ MissingConfig "missing dbfile path"
    Just db' -> Right $ Conf p' db'

parseOptions :: FilePath -> IO (Either ConfigError Conf)
parseOptions fp = do
  fc <- parseJSONConfigFile fp
  cc <- commandLineParser
  case fc of
    Left x -> return (Left x)
    Right fc' -> return $ makeConfig (defaultConf <> fc' <> cc)
