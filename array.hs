-- An APLish array is made of its shape (integers) and its actual elements
data Array a = Arr [Int] [a] deriving (Show, Read, Eq)
shape (Arr shp _) = shp
rank (Arr shp _) = length shp
elts (Arr _ xs) = xs

instance Functor Array where
  fmap f (Arr shp xs) = Arr shp (fmap f xs)

-- An APLish function has an expected argument rank and a function body which
-- operates on arrays. APL isn't very curry-friendly, but it is The Haskell Way,
-- so a function of multiple arguments should take one argument and then produce
-- an ArrayFun which will consume the next (at the appropriate expected rank),
-- and so on.
data ArrayFun a b = Fun Int (Array a -> Array b)
arg_rank (Fun r _) = r

-- TODO: Apply an individual ArrayFun
-- TODO: Apply an Array of ArrayFuns
