import Control.Applicative

data Option a = Some a | None deriving Show

instance Functor Option where
  fmap f None = None
  fmap f (Some x) = Some (f x)

instance Applicative Option where
  pure x = Some x
  (Some f) <*> (Some x) = Some (f x)
  None <*> _ = None
  _ <*> None = None 
