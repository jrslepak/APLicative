Here is a demonstration of a part of the "algebra" of array-oriented programming.
Generalizing APL-like array lifting semantics to include first-class functions means that lifting a function-producing function yields an array of functions, which may itself appear in function position.
The semantics of function application allows the function position "argument" to contribute to the application frame when the array of functions is applied to some arrays of arguments.

With a curried function, application accumulates a frame that may grow with each additional argument.
For example, applying a `3`-frame function array to a scalar-frame argument would produce a `3`-frame result array,
which might then be applied to a `3,2,4`-frame argument (growing to a `3,2,4`-frame result) and then that result to a `3,2`-frame argument (still with a `3,2,4`-frame final result).

The value of the final result is the same as if we had applied an uncurried version of the `3`-frame function array to scalar-frame, `3,2,4`-frame, and `3,2`-frame arguments.
This sort of "generalized map" for multiple-argument curried functions suggests treating the array structure as an applicative functor.
Though less transparent than Racket's `prop:procedure`, the `<*>` operator provides the hook for using a "custom function application" for arrays.
`<*>` converts the ordinary functions in the left array into array-level functions (with expected rank 0),
and then it invokes a function which performs the rank-polymorphic lifted application on the function and argument arrays.
`pure` constructs a scalar array around the given atom.

````
> pure (+) <*> (Arr [3,2] [10,20,30,40,50,60]) <*> (Arr [3] [1, 2, 3])
Arr [3,2] [11,21,32,42,53,63]
````

Demonstrating the `Applicative` laws:

````
> pure id <*> Arr [3] [2,4,6]
Arr [3] [2,4,6]
````

`id` gets applied to each individual element, and the array shape is left untouched.

````
> (pure (+1) :: (Array (Int -> Int))) <*> pure 5
Arr [] [6]
> pure ((+1) 5) :: (Array Int)
Arr [] [6]
````

Scalar-to-scalar application is the trivial lifting case:
the sole function atom is applied to the sole argument atom, producing another scalar array.

````
> Arr [2] [(+1), (*2)] <*> pure 5
Arr [2] [6,10]
> pure ($ 5) <*> Arr [2] [(+1), (*2)]
Arr [2] [6,10]
````

The lifting semantics treats each frame contribution equally.
Although only one array has the `2`-frame, it does not matter whether that array is the function or argument position.
The scalar is replicated to have its own `2`-frame, matching the argument atom up with the same function atoms.

````
> pure (.) <*> Arr [2] [(+3),(+2)] <*> Arr [] [(*2)] <*> (pure 3)
Arr [2] [9,8]
> Arr [2] [(+3),(+2)] <*> (Arr [] [(*2)] <*> (pure 3))
Arr [2] [9,8]
````

Function composition with `pure (.)` is a little odd in array-oriented programming because `(.)` gets lifted and applied to individual functions within two function arrays.
However, the flow of individual argument cells through the composed function atoms remains the same as if the function arrays had been applied one at a time.
The frame compatibility all works out because the join operation in the "prefix ordering" semilattice is associative.
