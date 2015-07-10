class Dionom d where
  dempty :: d
  dappend :: d -> d -> d
  dconcat :: [d] -> d
  dconcat = foldr dappend dempty

newtype Sums a     = Sums { getSum :: a }
newtype Products a = Products { getProduct :: a }

instance Dionom [a] where
  dempty = []
  dappend x y = x ++ y

instance Num a => Dionom (Sums a) where
  dempty = Sums 0
  dappend (Sums x) (Sums y) = Sums (x + y)

instance Num a => Dionom (Products a) where
  dempty = Products 1
  dappend (Products x) (Products y) = Products (x * y)

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show xs = "(" ++ helper xs ++ ")"
    where helper (Cons x xs) = show x ++ ", " ++ helper xs
          helper Nil = ""
