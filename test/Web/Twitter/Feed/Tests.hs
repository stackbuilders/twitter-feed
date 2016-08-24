module Web.Twitter.Feed.Tests where

import Test.HUnit hiding (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)

import Web.Twitter.Feed
import Web.Twitter.Types

addLinkTest :: Assertion
addLinkTest = addLink (Link 8 8 "not ") "this is a test" @?= "this is not a test"

addLinkEllipsisTest :: Assertion
addLinkEllipsisTest = addLink (Link 139 140 "link") tweet @?= expected
  where
    tweet    = replicate 130 'a' ++ " http://ww"
    expected = replicate 130 'a' ++ " link"

timelineUrlTest :: Assertion
timelineUrlTest = timelineUrl "someone" 3 True @?= expected
  where
    expected = "https://api.twitter.com/1.1/statuses/user_timeline.json" ++
               "?screen_name=someone&count=3&exclude_replies=true"

sortLinkTest :: Assertion
sortLinkTest =
  sortLinks urlEntitiesExamples userEntitiesExamples mediaEntitiesExamples
      @?= sortedLinks

tests :: Test
tests = testGroup "Web.Twitter.Feed"
          [ testCase "addLink"         addLinkTest
          , testCase "addLinkEllipsis" addLinkEllipsisTest
          , testCase "timelineUrl"     timelineUrlTest
          , testCase "sortLinks"       sortLinkTest
          ]

sortedLinks :: [Link]
sortedLinks =
  [ Link 2  12  "<a target=\"_blank\" href=\"google.com\">t.co/g</a>"
  , Link 13 20 "<a target=\"_blank\" href=\"//twitter.com/Hackage\">\
    \@Hackage</a>"
  , Link 20 39 "<a target=\"_blank\" href=\"stackbuilders.com\">t.co/s</a>"
  , Link 50 62 "<a target=\"_blank\" href=\"//twitter.com/StackBuilders\">\
    \@StackBuilders</a>"
  , Link 70 92 "<a target=\"_blank\" href=\"http://t.co/xhzEs8NuQa\">\
    \pic.twitter.com/xhzEs8NuQa</a>"
  ]

urlEntitiesExamples :: [URLEntity]
urlEntitiesExamples =
  [ URLEntity "google.com"        (2, 12)  "t.co/g"
  , URLEntity "stackbuilders.com" (20, 39) "t.co/s"
  ]

userEntitiesExamples :: [UserEntity]
userEntitiesExamples =
  [ UserEntity "Hackage"       (13, 20)
  , UserEntity "StackBuilders" (50, 62)
  ]

mediaEntitiesExamples :: [MediaEntity]
mediaEntitiesExamples =
  [ MediaEntity "http://t.co/xhzEs8NuQa" (70, 92) "pic.twitter.com/xhzEs8NuQa" ]
