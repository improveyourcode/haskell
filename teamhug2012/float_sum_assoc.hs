import Test.QuickCheck

prop_plus_assoc :: Float -> Float -> Float -> Bool
prop_plus_assoc x y z = (x + y) + z == x + (y + z)