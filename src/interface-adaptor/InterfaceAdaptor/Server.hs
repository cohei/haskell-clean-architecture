{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module InterfaceAdaptor.Server (application) where

import           Prelude                   hiding (return)

import           Control.Monad.Catch       (MonadThrow (throwM), catches, try)
import qualified Control.Monad.Catch       as E (Handler (Handler))
import           Control.Monad.Except      (ExceptT (ExceptT))
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Proxy                (Proxy (Proxy))
import           Servant.API               ((:<|>) ((:<|>)),
                                            NoContent (NoContent))
import           Servant.Server            (Application, Handler (Handler),
                                            ServantErr (errBody), ServerT,
                                            err400, err404, err412, hoistServer,
                                            serve)

import           Entity.Book               (Book, BookRepository (findAll))
import           Entity.Lending            (LendingRepository)
import           Entity.User               (User)
import           InterfaceAdaptor.API      (API, HealthCheck, Library)
import           InterfaceAdaptor.BookView (BookView, fromBook)
import           InterfaceAdaptor.InMemory (Env, InMemory, runInMemory)
import           UseCase.Borrowing         (CannotBorrow (Lent, Overdue, TooManyBooks))
import qualified UseCase.Borrowing         as B (borrow)
import           UseCase.Identify          (Id, Identify)
import           UseCase.Query             (NotFound (BookNotFound, UserNotFound))
import qualified UseCase.Returning         as R (return)

application :: Env -> Application
application env = serve api $ hoistServer api (convert env) server

api :: Proxy API
api = Proxy

convert :: Env -> InMemory a -> Handler a
convert env = Handler . ExceptT . try . (`catches` handlers) . runInMemory env
  where
    handlers :: MonadThrow m => [E.Handler m a]
    handlers = [cannotBorrowHandler, notFoundHandler]

server :: ServerT API InMemory
server = libraryServer :<|> healthCheckServer

healthCheckServer :: ServerT HealthCheck InMemory
healthCheckServer = pure NoContent

libraryServer :: ServerT Library InMemory
libraryServer = index :<|> borrow :<|> return

index :: (BookRepository m, Identify Book m, LendingRepository m) => m [BookView]
index = mapM fromBook =<< findAll

borrow ::
  (MonadIO m, MonadThrow m, Identify Book m, Identify User m, LendingRepository m) =>
  Id User -> Id Book -> m BookView
borrow userId bookId = fromBook =<< B.borrow userId bookId

return ::
  (MonadThrow m, Identify Book m, Identify User m, LendingRepository m) =>
  Id User -> Id Book -> m BookView
return userId bookId = fromBook =<< R.return userId bookId

cannotBorrowHandler :: MonadThrow m => E.Handler m a
cannotBorrowHandler = E.Handler $ throwM . convertError
  where
    convertError :: CannotBorrow -> ServantErr
    convertError TooManyBooks = err400 { errBody = "Too many books checked out" }
    convertError Overdue      = err400 { errBody = "You have books overdue!" }
    convertError Lent         = err412 { errBody = "Sorry, this book is not available" }

notFoundHandler :: MonadThrow m => E.Handler m a
notFoundHandler = E.Handler $ throwM . convertError
  where
    convertError :: NotFound -> ServantErr
    convertError BookNotFound = err404 { errBody = "Could not find book" }
    convertError UserNotFound = err404 { errBody = "Could not find user" }
