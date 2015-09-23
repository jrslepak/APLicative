{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module APL where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative

-- An APLish array is made of its shape (integers) and its actual elements
data Array a = Arr [Int] [a] deriving (Show, Read, Eq)
shape (Arr shp _) = shp
rank (Arr shp _) = length shp
elts (Arr _ xs) = xs
scalar x = Arr [] [x]

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
nest f_rank (Arr shp xs) =
  if (f_rank > length shp)
  then Nothing
  else let (f_shp, c_shp) = splitAt f_rank shp in
       let c_size = foldr (*) 1 c_shp in
       let cells = splits c_size xs in
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
type a :->: b = ArrayFun a b
arg_rank (Fun r _) = r

-- Apply an individual ArrayFun
fun_apply :: (a :->: b) -> Array a -> Maybe (Array b)
fun_apply (Fun r fn) arr = do
  -- First, split the argument array into frame-of-arg-cells
  fc <- nest r arr
  let arg_cells = elts fc
  let frm = shape fc
  -- Apply the function to each cell
  let ret_cells = map fn arg_cells
  -- Build the frame-of-result-cells
  let ret_nested = Arr frm ret_cells
  -- Produce the collapsed frame-of-result-cells
  collapse ret_nested


-- Apply an Array of ArrayFuns
arr_apply :: Array (a :->: b) -> Array a -> Maybe (Array b)
arr_apply fs as = do
  -- Check that all fs have same expected rank
  cell_rank <- unique_elt $ fmap arg_rank $ elts fs
  -- Check for shape compatibility
  let fn_frame = shape fs
  let arg_frame = take ((rank as) - cell_rank) (shape as)
  pr_frame <- if fn_frame `isPrefixOf` arg_frame
              then Just arg_frame
              else if arg_frame `isPrefixOf` fn_frame
                   then Just fn_frame
                   else Nothing
  -- Grow the arrays as needed
  fn_expanded <- frame_expand fn_frame pr_frame fs
  arg_expanded <- frame_expand arg_frame pr_frame as
  -- Pointwise application
  let fn_atoms = (elts fn_expanded)
  arg_nested <- nest (length pr_frame) arg_expanded
  let arg_cells = elts arg_nested
  ret_cells <- sequence $ zipWith fun_apply fn_atoms arg_cells
  collapse $ Arr pr_frame ret_cells
unsafe_apply :: Array (a :->: b) -> Array a -> Array b
unsafe_apply fs as = fromJust $ arr_apply fs as


-- Turn an ordinary Haskell function into an ArrayFun that operates on scalars
promote1 :: (a -> b) -> (a :->: b)
promote1 f = Fun 0 (fmap f)

scalfun :: (a -> b) -> Array (a :->: b)
scalfun f = Arr [] [promote1 f]

-- We can also do this for binary functions.
promote2 :: forall a b c. (a -> b -> c) -> (a :->: (b :->: c))
promote2 f = Fun 0 (\ as ->
                      (Arr []
                       [Fun 0 (\ bs ->
                                 (unsafe_apply
                                  (unsafe_apply (scalfun (promote1 . f)) as)
                                  bs))]))

-- A "promote" function can be written for any desired arity, but there's a more
-- concise solution. Instead of setting up all the function-to-ArrayFun
-- promotions statically, we can just work with ordinary functions until it's
-- time to apply them.
-- An (a -> b -> c -> d) can get converted to an (a :->: b -> c -> d), so that
-- application produces an array of (b -> c -> d), which can each be promoted to
-- a (b :->: c -> d), and so on. This accumulates a growing (or rather, a
-- non-shrinking) frame of result cells.
instance Applicative Array where
  pure x = Arr [] [x]
  f <*> xs = unsafe_apply (fmap promote1 f) xs
