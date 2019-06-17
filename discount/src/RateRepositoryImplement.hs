{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RateRepositoryImplement
  ( ConstantRate(runConstantRate)
  , runConstantRatePure
  , MockRate(runMockRate)
  , runMockRatePure
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor.Identity  (Identity (runIdentity))

import           Discounter             (Amount, Rate, RateRepository (rate))

constantRate :: Amount -> Rate
constantRate = const 0.05

newtype ConstantRate f a =
  ConstantRate { runConstantRate :: f a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Applicative f => RateRepository (ConstantRate f) where
  rate = ConstantRate . pure . constantRate

runConstantRatePure :: ConstantRate Identity a -> a
runConstantRatePure = runIdentity . runConstantRate

mockRate :: Amount -> Rate
mockRate amount
  | amount <=  100 = 0.01
  | amount <= 1000 = 0.02
  | otherwise      = 0.05

newtype MockRate f a =
  MockRate { runMockRate :: f a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Applicative f =>  RateRepository (MockRate f) where
  rate = MockRate . pure . mockRate

runMockRatePure :: MockRate Identity a -> a
runMockRatePure = runIdentity . runMockRate
