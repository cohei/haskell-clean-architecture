{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Discounter (Amount, Discount, Rate, discount, RateRepository(rate)) where

newtype Amount = Amount Double
  deriving newtype (Eq, Ord, Num, Read)

newtype Discount = Discount Double
  deriving newtype (Eq, Num, Show)

newtype Rate = Rate Double deriving (Num, Fractional)

calculateDiscount :: Amount -> Rate -> Discount
calculateDiscount (Amount a) (Rate r) = Discount $ a * r

discount :: (Functor f, RateRepository f) => Amount -> f Discount
discount amount = calculateDiscount amount <$> rate amount

class RateRepository f where
  rate :: Amount -> f Rate
