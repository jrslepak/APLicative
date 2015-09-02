module APL where

import Data.Maybe
import Data.List
import Control.Monad

-- An APLish array is made of its shape (integers) and its actual elements
data Array a = Arr [Int] [a] deriving (Show, Read, Eq)
shape (Arr shp _) = shp
rank (Arr shp _) = length shp
elts (Arr _ xs) = xs

instance Functor Array where
  fmap f (Arr shp xs) = Arr shp (fmap f xs)

-- Get the sole unique element of a list, if there is one
unique_elt :: Eq a => [a] -> Maybe a
unique_elt (x:xs) = foldM (\ old new -> if old == new then Just old else Nothing) x xs

-- Split a list into segments of a given length
splits :: Int -> [a] -> [[a]]
splits l xs | l >= length xs = [xs]
splits l xs = let (first, rest) = splitAt l xs in
  first : (splits l rest)

-- Break an array into frame-of-cells nested form
nest :: Int -> Array a -> Maybe (Array (Array a))
nest f_rank (Arr shp elts) =
  if (f_rank > length shp)
  then Nothing
  else let (f_shp, c_shp) = splitAt f_rank shp in
       let c_size = foldr (*) 1 c_shp in
       let cells = splits c_size elts in
       return $ Arr f_shp $ fmap (Arr c_shp) cells

-- Collapse a nested array into flat form
collapse :: Array (Array a) -> Maybe (Array a)
collapse (Arr f_shp cells) = do
  c_shp <- unique_elt $ fmap shape cells
  return $ Arr (f_shp ++ c_shp) (join $ fmap elts cells)

-- Expand an array from one frame to another
frame_expand :: [Int] -> [Int] -> Array a -> Maybe (Array a)
frame_expand lo hi (Arr _ xs) | isPrefixOf lo hi = -- split the elts according to cell count
  let hc_count = foldr (*) 1 hi
      lc_count = foldr (*) 1 lo in
  let growth = hc_count `div` lc_count in
  let c_size = length xs `div` lc_count in
  let cells = splits c_size xs in
  Just $ Arr hi (join $ join $ fmap (replicate growth) cells)
frame_expand _ _ _ = Nothing


-- An APLish function has an expected argument rank and a function body which
-- operates on arrays. APL isn't very curry-friendly, but it is The Haskell Way,
-- so a function of multiple arguments should take one argument and then produce
-- an ArrayFun which will consume the next (at the appropriate expected rank),
-- and so on.
data ArrayFun a b = Fun Int (Array a -> Array b)
arg_rank (Fun r _) = r

-- TODO: Apply an individual ArrayFun
-- TODO: Apply an Array of ArrayFuns
