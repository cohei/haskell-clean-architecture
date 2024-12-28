{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module UseCase.Returning (return) where

import Prelude hiding (return)

import Control.Monad.Catch (MonadThrow)

import Entity.Book (Book)
import Entity.Lending (LendingRepository (delete))
import Entity.User (User)
import UseCase.Identify (Id, Identify)
import UseCase.Query (queryBook, queryUser)

return ::
  (MonadThrow m, Identify Book m, Identify User m, LendingRepository m) =>
  Id User ->
  Id Book ->
  m Book
return userId bookId = do
  user <- queryUser userId
  book <- queryBook bookId
  delete user book
  pure book
