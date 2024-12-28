{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module InterfaceAdaptor.BookView (BookView (BookView), fromBook) where

import Prelude hiding (id)

import Data.Time.Clock (UTCTime)

import Entity.Book (Book)
import Entity.Book qualified as B (createdAt, title)
import Entity.Lending (LendingRepository, dueOf)
import UseCase.Identify (Id, Identify (identify))

data BookView
  = BookView
  { id :: Id Book,
    title :: String,
    status :: String,
    dueDate :: Maybe UTCTime,
    createdAt :: UTCTime
  }

fromBook :: (Identify Book m, LendingRepository m) => Book -> m BookView
fromBook book = do
  maybeDue <- dueOf book
  let (s, d) = maybe ("Available", Nothing) (("Borrowed",) . Just) maybeDue
  bookId <- identify book
  pure $
    BookView
      { id = bookId,
        title = B.title book,
        status = s,
        dueDate = d,
        createdAt = B.createdAt book
      }
