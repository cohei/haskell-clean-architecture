{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InterfaceAdaptor.InMemory (InMemory, runInMemory, Env, initialEnv) where

import Capability.Reader (MonadReader (MonadReader))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State
  ( Field (Field),
    HasState,
    ReaderIORef (ReaderIORef),
    gets,
    modify,
  )
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef)
import Data.IntMap (IntMap, fromList)
import Data.IntMap qualified as M
  ( elems,
    filter,
    keys,
    lookup,
    singleton,
  )
import Data.Maybe (listToMaybe, fromJust)
import Data.Set (Set)
import Data.Set qualified as S (empty, filter, insert, toList)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Entity.Book
  ( Book (Book, createdAt, title),
    BookRepository (findAll),
  )
import Entity.Lending (Lending (Lending), LendingRepository (delete, findByBook, findByUser, save))
import Entity.Lending qualified as L (book, user)
import Entity.User (User (User, createdAt, name))
import UseCase.Identify (Id (Id), Identify (identify, query))

newtype InMemory a = InMemory (IORef Env -> IO a)
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadThrow)
    via ReaderT (IORef Env) IO
  deriving
    (HasSource "bookStore" BookStore, HasSink "bookStore" BookStore, HasState "bookStore" BookStore)
    via Field "bookStore" "env" (ReaderIORef (MonadReader (ReaderT (IORef Env) IO)))
  deriving
    (HasSource "userStore" UserStore, HasSink "userStore" UserStore, HasState "userStore" UserStore)
    via Field "userStore" "env" (ReaderIORef (MonadReader (ReaderT (IORef Env) IO)))
  deriving
    (HasSource "lendingStore" LendingStore, HasSink "lendingStore" LendingStore, HasState "lendingStore" LendingStore)
    via Field "lendingStore" "env" (ReaderIORef (MonadReader (ReaderT (IORef Env) IO)))

data Env
  = Env
  { bookStore :: BookStore,
    userStore :: UserStore,
    lendingStore :: LendingStore
  }
  deriving (Generic)

type BookStore = IntMap Book

type UserStore = IntMap User

type LendingStore = Set Lending

deriving instance Ord Book

deriving instance Ord Lending

deriving instance Ord User

runInMemory :: Env -> InMemory a -> IO a
runInMemory env (InMemory f) = f =<< newIORef env

initialEnv :: IO Env
initialEnv = do
  now <- getCurrentTime
  pure
    Env
      { bookStore = initialBooks now,
        userStore = initialUsers now,
        lendingStore = initialLendings
      }

initialBooks :: UTCTime -> BookStore
initialBooks now = fromList [(1, book1), (2, book2)]
  where
    book1 = Book {title = "", createdAt = now}
    book2 = Book {title = "Book", createdAt = now}

initialUsers :: UTCTime -> UserStore
initialUsers now = M.singleton 1 User {name = "Dasha", createdAt = now}

initialLendings :: LendingStore
initialLendings = S.empty

instance BookRepository InMemory where
  findAll = gets @"bookStore" M.elems

instance LendingRepository InMemory where
  findByBook book = gets @"lendingStore" $ S.toList . S.filter ((== book) . L.book)

  findByUser user = gets @"lendingStore" $ S.toList . S.filter ((== user) . L.user)

  save = modify @"lendingStore" . S.insert

  delete user book = modify @"lendingStore" $ S.filter (not . isTarget)
    where
      isTarget lending = L.user lending == user && L.book lending == book

instance Identify Book InMemory where
  identify book = gets @"bookStore" $ Id . elemKey book

  query = gets @"bookStore" . M.lookup . coerce

instance Identify User InMemory where
  identify user = gets @"userStore" $ Id . elemKey user

  query = gets @"userStore" . M.lookup . coerce

elemKey :: Eq a => a -> IntMap a -> Int
elemKey v = fromJust . listToMaybe . M.keys . M.filter (== v)
