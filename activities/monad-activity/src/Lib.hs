module Lib
    ( Counter(Counter)
    ) where


data Counter a = Counter a Int
    deriving (Show,Eq)

instance Functor Counter where
    fmap f (Counter a i) = Counter (f a) i

instance Applicative Counter where
    pure x = Counter x 0
    (Counter f i) <*> (Counter x j) = Counter (f x) (i + j)

instance Monad Counter where
    return x = Counter x 0
    Counter a i >>= f =
        let (Counter x y) = f a
        in (Counter x (i + y + 1))
