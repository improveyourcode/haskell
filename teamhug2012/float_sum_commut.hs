import Test.QuickCheck

prop_float_sum_commutes :: Float -> Float -> Bool
prop_float_sum_commutes a b = a + b == b + a