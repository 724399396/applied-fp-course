{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module FirstApp.AppM where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))
import           Control.Monad (join)

import           Data.Text              (Text)

import           FirstApp.DB.Types      (FirstAppDB)
import           FirstApp.Types         (Conf, Error)

import           Data.Bifunctor         (first)

data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will expand the capabilities of our AppM by including the
-- Either type in our definition. We will also rework our Monad instance to stop
-- processing when it encounters a Left value.
--
-- This will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value.
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     alsoMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (Env -> IO (Either Error a))
  deriving Functor

-- The runAppM function only needs to change the final return type as it has an
-- 'Either Error' and not just the 'a'.
runAppM
  :: AppM a
  -> Env
  -> IO (Either Error a)
runAppM (AppM m) =
  m

-- Copy over your previously completed definitions.

instance Applicative AppM where
  pure :: a -> AppM a
  pure a  = AppM $ \_ -> pure $ pure a

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  ab <*> a = AppM $ \e -> (\ef ea -> ef <*> ea) <$> (runAppM ab e) <*> (runAppM a e)

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  a >>= fa = AppM $ \e -> do a' <- runAppM a e
                             join <$> traverse (\a'' -> runAppM (fa a'') e) a'

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO ia = AppM $ \_ -> fmap pure ia

instance MonadReader Env AppM where
  ask :: AppM Env
  ask = AppM (return . return)

  local :: (Env -> Env) -> AppM a -> AppM a
  local f a = AppM $ \e -> runAppM a (f e)

  reader :: (Env -> a) -> AppM a
  reader ea = AppM $ \e -> return $ return $ (ea e)

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError e = AppM $ \_ -> return $ Left e

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError am ac = AppM $ \e ->
    join $ flip runAppM e <$> (either ac return <$> runAppM am e)

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither = either throwError pure
