module Entity.Lending
  ( Lending (Lending, book, user, due),
    LendingRepository (findByBook, findByUser, save, delete),
    isTooManyBooks,
    isOverdue,
    dueOf,
    isBorrowed,
  )
where

import Data.Maybe (listToMaybe)
import Data.Time.Clock (UTCTime)

import Entity.Book (Book)
import Entity.User (User)

data Lending
  = Lending {user :: User, book :: Book, due :: UTCTime}
  deriving (Eq)

class (Monad m) => LendingRepository m where
  findByBook :: Book -> m [Lending]
  findByUser :: User -> m [Lending]
  save :: Lending -> m ()
  delete :: User -> Book -> m ()

isBorrowed :: (LendingRepository m) => Book -> m Bool
isBorrowed = fmap (not . null) . findByBook

dueOf :: (LendingRepository m) => Book -> m (Maybe UTCTime)
dueOf = fmap (fmap due . listToMaybe) . findByBook

isOverdue :: (LendingRepository m) => UTCTime -> User -> m Bool
isOverdue now = fmap (any ((< now) . due)) . findByUser

isTooManyBooks :: (LendingRepository m) => User -> m Bool
isTooManyBooks = fmap ((> borrowLimit) . length) . findByUser

borrowLimit :: Int
borrowLimit = 10
