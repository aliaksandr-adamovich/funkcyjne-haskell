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
