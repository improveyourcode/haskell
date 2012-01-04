import Control.Applicative

dist :: Applicative f => [f a] -> f [a]
dist [] = pure []
dist (f : fs) = (:) <$> f <*> dist fs

traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse f [] = pure []
traverse f (x:xs) = (:) <$> f x <*> traverse f xs
