module CLI (cli) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (hFlush, stdout)

import Discounter
  ( Amount,
    Discount,
    RateRepository,
    discount,
  )

cli :: (RateRepository m, MonadIO m) => m ()
cli = do
  amount <- liftIO getAmount
  d <- discount amount
  liftIO $ putDiscount d

getAmount :: IO Amount
getAmount = do
  putStrLn "Input amount"
  putStr "> "
  hFlush stdout
  readLn

putDiscount :: Discount -> IO ()
putDiscount d = putStrLn $ "Discount: " ++ show d
