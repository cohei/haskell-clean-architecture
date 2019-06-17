module Entity.User (User(User, name, createdAt)) where

import           Data.Time.Clock (UTCTime)

data User =
  User
  { name      :: String
  , createdAt :: UTCTime
  } deriving (Eq)
