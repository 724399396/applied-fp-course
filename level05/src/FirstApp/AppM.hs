{-# LANGUAGE DeriveFunctor #-}
module FirstApp.AppM (liftEither, AppM) where

import FirstApp.Types (Error)

newtype AppM a = AppM (IO (Either Error a)) deriving Functor

instance Applicative AppM where
  pure = AppM . pure . pure
  (AppM iea) <*> (AppM ief) = AppM (iea <*> ief)

liftEither :: Either Error a -> AppM a
liftEither = AppM . return
