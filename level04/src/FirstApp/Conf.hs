module FirstApp.Conf where

data Conf = Conf {
  dbFilePath :: FilePath
                 }

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db"
