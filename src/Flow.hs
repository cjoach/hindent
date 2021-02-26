module Flow
    ( (|>)
    , (<|)
    ) where


import Data.Function ((&))


infixl 0 |>


(|>) :: a -> (a -> b) -> b
(|>) =
    (&)


infixr 0 <|


(<|) :: (a -> c) -> a -> c
(<|) =
    ($)
