module Entity.Book
  ( Book(Book, title, createdAt)
  , BookRepository(findAll)
  ) where

import           Data.Time.Clock (UTCTime)

data Book =
  Book
  { title     :: String
  , createdAt :: UTCTime
  } deriving (Eq)

class Monad m => BookRepository m where
  findAll :: m [Book]
