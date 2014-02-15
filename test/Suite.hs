module Main where

import  Test.Framework (defaultMain)

import qualified Web.Twitter.Feed.Tests

main :: IO ()
main = defaultMain
    [ Web.Twitter.Feed.Tests.tests ]
