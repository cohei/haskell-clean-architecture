{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Test.Hspec
  ( Spec,
    context,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Test.Main
  ( ProcessResult (ProcessResult, prException, prStdout),
    captureProcessResult,
    withStdin,
  )

import CLI (cli)
import Discounter (Discount, discount)
import RateRepositoryImplement
  ( ConstantRate (runConstantRate),
    MockRate (runMockRate),
    runConstantRatePure,
    runMockRatePure,
  )

main :: IO ()
main = hspec $ do
  describe "Discounter" $
    describe "discount" $ do
      context "constant rate" $ do
        context "amount 100" $
          it "returns discount" $
            runConstantRatePure (discount 100) `shouldBe` (5 :: Discount)

        context "amount 200" $
          it "returns discount" $
            runConstantRatePure (discount 200) `shouldBe` (10 :: Discount)

      context "mock rate" $ do
        context "amount 100" $
          it "returns discount" $
            runMockRatePure (discount 100) `shouldBe` (1 :: Discount)

        context "amount 200" $
          it "returns discount" $
            runMockRatePure (discount 200) `shouldBe` (4 :: Discount)

  describe "CLI" $
    describe "cli" $ do
      context "constant rate" $ do
        context "amount 100" $
          it "returns discount" $
            (runConstantRate cli, "100") `shouldReturnDiscount` "5.0"

        context "amount 200" $
          it "returns discount" $
            (runConstantRate cli, "200") `shouldReturnDiscount` "10.0"

      context "mock rate" $ do
        context "amount 100" $
          it "returns discount" $
            (runMockRate cli, "100") `shouldReturnDiscount` "1.0"

        context "amount 200" $
          it "returns discount" $
            (runMockRate cli, "200") `shouldReturnDiscount` "4.0"

shouldReturnDiscount :: (IO (), ByteString) -> ByteString -> IO ()
shouldReturnDiscount (command, amount) discount = do
  ProcessResult {prException, prStdout} <- captureProcessResult $ withStdin (amount <> "\n") command
  maybe (pure ()) throwIO prException
  prStdout `shouldBe` "Input amount\n> Discount: " <> discount <> "\n"
