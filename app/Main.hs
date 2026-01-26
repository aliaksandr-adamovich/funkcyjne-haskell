{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as TL

import Lib

main :: IO ()
main = scotty 3000 $ do
  get "/" $
    html "<h1>OK</h1>"

  get "/testIsSorted" $ do
    let ok1 = isSorted (<=) [1,2,2,4]
    let ok2 = isSorted (<=) [1,3,2]
    text (TL.pack (show (ok1, ok2)))

  get "/add/:a/:b" $ do
    a <- param "a"
    b <- param "b"
    json (addOrSub True a b)

  get "/sub/:a/:b" $ do
    a <- param "a"
    b <- param "b"
    json (addOrSub False a b)

  post "/concat" $ do
    (a, b, c) <- jsonData :: ActionM ([Int], [Int], [Int])
    json (concatThree a b c)

  post "/sum" $ do
    xs <- jsonData
    json (sumOrNil xs)

  get "/state/int/:s" $ do
    s <- param "s"
    json (stateInt s)

  get "/state/double/:s" $ do
    s <- param "s"
    json (stateDouble s)

  get "/state/pairs/:s" $ do
    s <- param "s"
    json (statePairs s)

  get "/state/double/map/:s" $ do
    s <- param "s"
    json (stateDoubleMap s)

  get "/state/double/flatmap/:s" $ do
    s <- param "s"
    json (stateDoubleFlatMap s)
