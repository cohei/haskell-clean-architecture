{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module InterfaceAdaptor.API (API, HealthCheck, Library) where

import           Data.Aeson                (ToJSON)
import           GHC.Generics              (Generic)
import           Servant.API               ((:<|>), (:>), Capture, Get,
                                            GetNoContent, JSON, Post)
import           Web.HttpApiData           (FromHttpApiData)

import           Entity.Book               (Book)
import           Entity.User               (User (User))
import           InterfaceAdaptor.BookView (BookView (BookView))
import           UseCase.Identify          (Id (Id))

type API = Library :<|> HealthCheck

type HealthCheck = "status" :> GetNoContent

type Library =
  "index" :> Get '[JSON] [BookView] :<|>
  "borrow" :> Capture "userId" (Id User) :> Capture "bookId" (Id Book) :> Post '[JSON] BookView :<|>
  "return" :> Capture "userId" (Id User) :> Capture "bookId" (Id Book) :> Post '[JSON] BookView

deriving instance FromHttpApiData (Id a)
deriving instance Generic BookView
deriving instance Generic User
deriving instance ToJSON (Id a)
instance ToJSON BookView
instance ToJSON User
