{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module UseCase.Borrowing
  ( borrow
  , CannotBorrow(TooManyBooks, Overdue, Lent)
  ) where

import           Control.Exception      (Exception)
import           Control.Monad.Catch    (MonadThrow (throwM))
import           Control.Monad.Extra    (whenM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Time.Clock        (UTCTime, getCurrentTime)

import           Entity.Book            (Book)
import           Entity.Lending         (Lending (Lending, book, due, user),
                                         LendingRepository (save), isBorrowed,
                                         isOverdue, isTooManyBooks)
import           Entity.User            (User)
import           UseCase.Identify       (Id, Identify)
import           UseCase.Query          (queryBook, queryUser)

data CannotBorrow =
  TooManyBooks | Overdue | Lent
  deriving (Show, Exception)

borrow ::
  (MonadIO m, MonadThrow m, Identify Book m, Identify User m, LendingRepository m) =>
  Id User -> Id Book -> m Book
borrow userId bookId = do
  now <- liftIO getCurrentTime
  user <- queryUser userId
  canUserBorrow now user
  book <- queryBook bookId
  canBookBeBorrowed book
  save Lending { due = now, .. }
  pure book

canUserBorrow :: (MonadThrow m, LendingRepository m) => UTCTime -> User -> m ()
canUserBorrow now user = do
  whenM (isOverdue now user) $ throwM Overdue
  whenM (isTooManyBooks user) $ throwM TooManyBooks

canBookBeBorrowed :: (MonadThrow m, LendingRepository m) => Book -> m ()
canBookBeBorrowed book = whenM (isBorrowed book) $ throwM Lent
