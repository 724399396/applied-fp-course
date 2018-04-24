{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FirstApp.AppM where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           FirstApp.Types         (Conf, FirstAppDB)

-- First, let's clean up our (Conf,FirstAppDB) with an application Env type. We
-- will add a general purpose logging function as well. Remember that functions
-- are values, we're able to pass them around and place them on records like any
-- other type.
data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- It would be nice to remove the need to pass around our Env to every function
-- that needs it. Wouldn't it be great to have our functions run where we could
-- simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:

newtype AppM a = AppM ( Env -> IO a )

-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
--
-- This structure allows us to start writing our functions in terms of
-- constraints. As an example, if we wanted to abstract over IO and indicate
-- that instead of the concrete type we wanted a constraint that allows for IO
-- actions. Our AppM would look more like this:
--
-- AppM m a = AppM ( Env -> m a )
--
-- Then our functions would look like:
--
-- foo :: MonadIO m => Int -> AppM m a
--
-- Or we could not use a concrete type for Env
--
-- AppM e m a = AppM ( e -> m a )
--

runAppM
  :: AppM a
  -> Env
  -> IO a
runAppM (AppM a) e =
  a e

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  fmap f aa = AppM $ \e -> fmap f (runAppM aa e)

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM $ \_ -> pure a

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  ab <*> a = AppM $ \e -> (runAppM ab e) <*> (runAppM a e)

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  -- When it comes to running functions in AppM as a Monad, this will take care
  -- of passing the Env from one function to the next.
  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  a >>= fa = AppM $ \e -> do a' <- runAppM a e
                             runAppM (fa a') e

instance MonadReader Env AppM where
  -- Return the current Env from the AppM.
  ask :: AppM Env
  ask = AppM return

  -- Run a AppM inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM a -> AppM a
  local f a = AppM $ \e -> runAppM a (f e)

  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM a
  reader ea = AppM $ \e -> return (ea e)

instance MonadIO AppM where
  -- Take a type of 'IO a' and lift it into our AppM.
  liftIO :: IO a -> AppM a
  liftIO ia = AppM $ \_ -> ia

-- Move on to ``src/FirstApp/DB.hs`` after this
