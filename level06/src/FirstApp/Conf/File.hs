{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf.File (parseJSONConfigFile) where

import FirstApp.Types (ConfigError (..), PartialConf)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (catch)
import Data.Aeson (eitherDecode)
import Data.Bifunctor (first)

--
-- | readConfFile
-- >>> readConfFile "badFileName.no"
-- Left (ReadFileError "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "test.json"
-- Right "{\"foo\":33}\n"
--
readConfFile :: FilePath -> IO (Either ConfigError ByteString)
readConfFile fp = (catch :: IO a -> (IOError -> IO a) -> IO a) (Right <$> LBS.readFile fp) (pure . Left . ReadFileError . show)

parseJSONConfigFile :: FilePath -> IO (Either ConfigError PartialConf)
parseJSONConfigFile fp = ((first DecodeConfError . eitherDecode) =<<) <$> readConfFile fp
