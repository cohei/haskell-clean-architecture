{-# LANGUAGE MultiParamTypeClasses #-}
module UseCase.Identify (Id(Id), Identify(identify, query)) where

newtype Id a =
  Id Int
  deriving (Eq, Ord)

class Identify a f where
  identify :: a -> f (Id a)
  query    :: Id a -> f (Maybe a)
