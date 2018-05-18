{-# LANGUAGE DeriveFunctor #-}
module FirstApp.AppM (liftEither, liftToAppM, AppM, runAppM) where

import FirstApp.Types (Error)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype AppM a = AppM (IO (Either Error a)) deriving Functor

runAppM :: AppM a -> IO (Either Error a)
runAppM (AppM a) = a

instance Applicative AppM where
  pure = AppM . pure . pure
  (AppM iea) <*> (AppM ief) = AppM (liftA2 (<*>) iea ief)

instance Monad AppM where
  return = pure
  (AppM iea) >>= f = AppM $ iea >>= \ea -> case ea of
                                             Left a -> return $ Left a
                                             Right x -> runAppM (f x)

instance MonadIO AppM where
  liftIO = AppM . fmap Right

liftEither :: Either Error a -> AppM a
liftEither = AppM . return

liftToAppM :: IO (Either Error a) -> AppM a
liftToAppM = AppM
