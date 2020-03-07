module Lib
    ( someFunc
    ) where

import Control.Monad

someFunc :: IO ()
someFunc = forM_ (map fizzbuzz [0..100]) $ \result -> 
    putStrLn result

fizzbuzz :: Integer -> String
fizzbuzz num =
    case (mod num 3, mod num 5) of
        (0, 0) -> "Fizz Buzz"
        (0, _) -> "Fizz"
        (_, 0) -> "Buzz"
        (_, _) -> show num
