{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Web.Twitter.Feed
-- License     : MIT (see the file LICENSE)
-- Maintainer  : Justin Leitgeb <justin@stackbuilders.com>
--
-- Functions for fetching (and linkifying) timelines of Twitter users.
--
-----------------------------------------------------------------------------

module Web.Twitter.Feed
  ( timeline
  , addLink
  , timelineUrl
  , sortLinks
  ) where

import qualified Data.ByteString.Lazy as BS

import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.List (elemIndices, sort)
import Data.Char (toLower)
import Web.Twitter.Types

-- | Get and decode a user timeline.
timeline :: OAuth                            -- ^ OAuth client (consumer)
         -> Credential                       -- ^ OAuth credential
         -> Int                              -- ^ Count
         -> Bool                             -- ^ Exclude replies?
         -> String                           -- ^ Screen name
         -> IO (Either String [SimpleTweet]) -- ^ Either an error or
                                             --   the list of tweets
timeline oauth credential count excludeReplies username = do
  req <- createRequest username count excludeReplies
  res <- getResponse oauth credential req
  return $
    case decodeTweets res of
      Left message -> Left message
      Right ts     -> Right $ map (simplifyTweet . linkifyTweet) ts

createRequest :: String -> Int -> Bool -> IO Request
createRequest username count excludeReplies = parseUrlThrow $ timelineUrl username count excludeReplies

getResponse :: OAuth -> Credential -> Request -> IO (Response BS.ByteString)
getResponse oauth credential req = do
  m <- newManager tlsManagerSettings
  signedreq <- signOAuth oauth credential req
  httpLbs signedreq m

decodeTweets :: Response BS.ByteString -> Either String [Tweet]
decodeTweets = eitherDecode . responseBody

-- | Returns the URL for requesting a user timeline.
timelineUrl :: String -- ^ Screen name
            -> Int    -- ^ Count
            -> Bool   -- ^ Exclude replies?
            -> String -- ^ The URL for requesting the user timeline
timelineUrl user count excludeReplies =
    "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++
    user ++ "&count=" ++ show count ++ "&exclude_replies=" ++
    map toLower (show excludeReplies)

linkifyTweet :: Tweet -> Tweet
linkifyTweet tweet = Tweet (processText (text tweet)
                                        (userEntities  $ entities tweet)
                                        (urlEntities   $ entities tweet)
                                        (mediaEntities $ entities tweet))
                           (createdAt tweet)
                           (idStr tweet)
                           (entities tweet)

processText :: String -> [UserEntity] -> [URLEntity] -> [MediaEntity] -> String
processText message users urls medias = foldr addLink message sortedLinks
  where sortedLinks = sortLinks urls users medias

-- | Sort lists of URL, user mention, and media entities.
sortLinks :: [URLEntity]   -- ^ A list of URL entities
          -> [UserEntity]  -- ^ A list of user mention entities
          -> [MediaEntity] -- ^ A list of media entities
          -> [Link]        -- ^ The sorted list of links
sortLinks urls users medias = sort (map makeURLLink   urls ++
                                    map makeUserLink  users ++
                                    map makeMediaLink medias)

makeURLLink :: URLEntity -> Link
makeURLLink urlEntity = Link x y url
  where (x, y)  = urlIndices urlEntity
        urlText = displayUrl urlEntity
        href    = urlMessage urlEntity
        url     = "<a target=\"_blank\" href=\"" ++ href ++ "\">"
                    ++ urlText ++ "</a>"

makeMediaLink :: MediaEntity -> Link
makeMediaLink mediaEntity = Link x y url
  where (x, y)  = mediaIndices mediaEntity
        urlText = displayMediaUrl mediaEntity
        href    = mediaUrl mediaEntity
        url     = "<a target=\"_blank\" href=\"" ++ href ++ "\">"
                    ++ urlText ++ "</a>"

makeUserLink :: UserEntity -> Link
makeUserLink userEntity = Link x y mention
  where (x, y)   = userIndices userEntity
        username = screenName userEntity
        mention  = "<a target=\"_blank\" href=\"//twitter.com/" ++ username
                   ++ "\">@" ++ username ++ "</a>"

simplifyTweet :: Tweet -> SimpleTweet
simplifyTweet tweet =
    SimpleTweet { body = text tweet
                , tweetId = idStr tweet
                , created_at = createdAt tweet }

-- | Add a link to the text of a tweet.
addLink :: Link   -- ^ A link
        -> String -- ^ The text of a tweet without the link
        -> String -- ^ The text of the tweet with the link
addLink (Link 139 140 link) tweet = before ++ " " ++ link ++ after
  where before = fst (splitAt (lastBlank tweet) tweet)
        after  = snd (splitAt 140 tweet)
        lastBlank = last . elemIndices ' '
addLink (Link start end link) tweet = before ++ link ++ after
  where before = fst (splitAt start tweet)
        after  = snd (splitAt end tweet)
