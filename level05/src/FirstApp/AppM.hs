{-# LANGUAGE DeriveFunctor #-}
module FirstApp.AppM (liftEither, AppM) where

import FirstApp.Types (Error)
import Control.Applicative (liftA2)

newtype AppM a = AppM (IO (Either Error a)) deriving Functor

instance Applicative AppM where
  pure = AppM . pure . pure
  (AppM iea) <*> (AppM ief) = AppM (liftA2 (<*>) iea ief)

instance Monad AppM where
  return = pure
  (AppM iea) >>= f = AppM $ iea >>= \ea -> case ea of
                                             Left a -> return $ Left a
                                             Right x -> f x

liftEither :: Either Error a -> AppM a
liftEither = AppM . return
