{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FirstApp.AppM (liftEither, liftToAppM, AppM, runAppM, Env (..)) where

import           Control.Applicative    (liftA2)
import           Control.Monad.Except   (MonadError, catchError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask, local)
import           FirstApp.Types         (Conf, Error, FirstAppDB)

data Env = Env
  { envConf :: Conf
  , envDB :: FirstAppDB
  }

newtype AppM a = AppM (Env -> IO (Either Error a)) deriving Functor

runAppM :: AppM a -> Env -> IO (Either Error a)
runAppM (AppM a) = a

instance Applicative AppM where
  pure = AppM . pure . pure . pure
  (AppM iea) <*> (AppM ief) = AppM (liftA2 (liftA2 (<*>)) iea ief)

instance Monad AppM where
  return = pure
  ariea >>= f = AppM $ \conf -> runAppM ariea conf >>= \ea -> case ea of
                                                                Left a -> return $ Left a
                                                                Right x -> runAppM (f x) conf

instance MonadIO AppM where
  liftIO = AppM . pure . fmap Right

instance MonadReader Env AppM where
  ask :: AppM Env
  ask = AppM $ \c -> pure $ pure $ c
  local :: (Env -> Env) -> AppM a -> AppM a
  local f ae = AppM $ \c -> runAppM ae (f c)

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError e = AppM $ \_ -> pure $ Left e
  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError am f = AppM $ \c -> runAppM am c >>= \ea -> case ea of
                                                           Left a -> runAppM (f a) c
                                                           Right x -> pure $ pure x

liftEither :: Either Error a -> AppM a
liftEither = AppM . pure . pure

liftToAppM :: IO (Either Error a) -> AppM a
liftToAppM = AppM . pure
