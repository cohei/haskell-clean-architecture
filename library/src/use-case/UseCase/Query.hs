{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module UseCase.Query
  ( queryBook,
    queryUser,
    NotFound (BookNotFound, UserNotFound),
  )
where

import Control.Exception (Exception)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow (throwM))

import Entity.Book (Book)
import Entity.User (User)
import UseCase.Identify (Id, Identify (query))

queryBook :: (MonadThrow m, Identify Book m) => Id Book -> m Book
queryBook = maybeThrow BookNotFound <=< query

queryUser :: (MonadThrow m, Identify User m) => Id User -> m User
queryUser = maybeThrow UserNotFound <=< query

maybeThrow :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

data NotFound
  = UserNotFound
  | BookNotFound
  deriving (Show, Exception)
