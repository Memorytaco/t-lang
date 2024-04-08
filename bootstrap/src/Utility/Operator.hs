module Utility.Operator
    (
        (<!>)
    )
where


infixl 4 <!>

-- strong application operator
(<!>) :: (a -> b -> c) -> (a -> b) -> a -> c
f <!> g = \a -> f a (g a)