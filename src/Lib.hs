{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Aeson
import Network.HTTP.Simple

data HttpbinIpResponse = HttpbinIpResponse { origin :: String } deriving Show

instance FromJSON HttpbinIpResponse where
    parseJSON (Object v) = HttpbinIpResponse <$> v .: "origin"
    parseJSON _ = mempty

someFunc :: IO ()
someFunc = do
    res <- httpLBS "https://httpbin.org/ip"
    let ip = decode (getResponseBody res) :: Maybe HttpbinIpResponse
    case ip of
        Nothing -> putStrLn "failure"
        Just ip -> putStrLn (origin ip)

fizzbuzz :: Integer -> String
fizzbuzz num =
    case (mod num 3, mod num 5) of
        (0, 0) -> "Fizz Buzz"
        (0, _) -> "Fizz"
        (_, 0) -> "Buzz"
        (_, _) -> show num
