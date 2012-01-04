import Control.Applicative
import Data.Traversable
import Data.Monoid

newtype Id a = An {an :: a} deriving Show

newtype Accy o a = Acc {acc :: o}

instance Functor Id where
 fmap f (An x) = An (f x)

instance Applicative Id where
  pure = An
  An f <*> An x = An (f x)

instance Monoid o => Functor (Accy o) where
  fmap _ accy = Acc (acc accy)

instance Monoid o => Applicative (Accy o) where
  pure _ = Acc mempty
  Acc o1 <*> Acc o2 = Acc (o1 `mappend` o2)

accumulate :: (Traversable t, Monoid o) => (a -> o) -> t a -> o
accumulate f = acc . traverse (Acc . f)

reduce :: (Traversable t, Monoid o) => t o -> o
reduce = accumulate id
