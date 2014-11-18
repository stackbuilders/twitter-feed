{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Web.Twitter.Feed
  ( Tweet
  , SimpleTweet (..)
  , Link (..)
  , URLEntity (..)
  , UserEntity (..)
  , MediaEntity (..)
  , timeline
  , addLink
  , timelineUrl
  , sortLinks
  )
where

import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import GHC.Generics (Generic)
import Control.Applicative
import Data.List (sortBy)
import Control.Monad (mzero)
import Data.Char (toLower)

type BoundingIndices = [Int]

data SimpleTweet = SimpleTweet
    { body       :: String
    , tweetId    :: String
    , created_at :: String
    } deriving (Show, Generic)

data Tweet =  Tweet
  { text       :: String
  , createdAt  :: String
  , idStr      :: String
  , entities   :: Entities
  } deriving (Show, Generic)

data UserEntity = UserEntity
  { screenName  :: String
  , userIndices :: BoundingIndices
  } deriving (Show, Generic)

data URLEntity = URLEntity
  { urlMessage   :: String
  , urlIndices   :: BoundingIndices
  , displayUrl   :: String
  } deriving (Show, Generic)

data MediaEntity = MediaEntity
  { mediaUrl        :: String
  , mediaIndices    :: BoundingIndices
  , displayMediaUrl :: String
  } deriving (Show, Generic)

data Entities = Entities
  { userEntities  :: [UserEntity]
  , urlEntities   :: [URLEntity]
  , mediaEntities :: [MediaEntity]
  } deriving (Show, Generic)

data Link = Link
  { startIndex :: Int
  , endIndex   :: Int
  , newHtml    :: String
  } deriving (Show, Eq)

instance ToJSON SimpleTweet

instance ToJSON Tweet
instance FromJSON Tweet
  where parseJSON (Object v) = Tweet
                               <$> v .: "text"
                               <*> v .: "created_at"
                               <*> v .: "id_str"
                               <*> v .: "entities"
        parseJSON _          = mzero

instance ToJSON UserEntity
instance FromJSON UserEntity
  where parseJSON (Object v) = UserEntity
                               <$> v .: "screen_name"
                               <*> v .: "indices" .!= []
        parseJSON _          = mzero

instance ToJSON URLEntity
instance FromJSON URLEntity
  where parseJSON (Object v) = URLEntity
                               <$> v .: "url"
                               <*> v .: "indices" .!= []
                               <*> v .: "display_url"
        parseJSON _          = mzero

instance ToJSON MediaEntity
instance FromJSON MediaEntity
  where parseJSON (Object v) = MediaEntity
                               <$> v .: "url"
                               <*> v .: "indices" .!= []
                               <*> v .: "display_url"
        parseJSON _          = mzero

instance ToJSON Entities
instance FromJSON Entities
  where parseJSON (Object v) = Entities
                               <$> v .:? "user_mentions" .!= []
                               <*> v .:? "urls" .!= []
                               <*> v .:? "media" .!= []
        parseJSON _          = mzero

timeline :: OAuth -> Credential -> Int -> Bool -> String ->
            IO (Either String [SimpleTweet])
timeline oauth credential count excludeReplies username = do
  req <- parseUrl $ timelineUrl username count excludeReplies
  res <- withManager $ \m -> do
           signedreq <- signOAuth oauth credential req
           httpLbs signedreq m

  let decoded = decode $ responseBody res

  case decoded of
    Nothing -> return $ Left "Unable to retrieve tweets!"
    Just ts -> return $ Right $ map (simplifyTweet . linkifyTweet) ts

timelineUrl :: String -> Int -> Bool -> String
timelineUrl user count excludeReplies =
    "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++
    user ++ "&count=" ++ show count ++ "&exclude_replies=" ++
    (map toLower $ show excludeReplies)

linkifyTweet :: Tweet -> Tweet
linkifyTweet tweet = Tweet (processText (text tweet)
                                        (userEntities  $ entities tweet)
                                        (urlEntities   $ entities tweet)
                                        (mediaEntities $ entities tweet))
                           (createdAt tweet)
                           (idStr tweet)
                           (entities tweet)

processText :: String -> [UserEntity] -> [URLEntity] -> [MediaEntity] -> String
processText message users urls medias = foldr addLink message
                                          (sortLinks urls users medias)

sortLinks :: [URLEntity] -> [UserEntity] -> [MediaEntity] -> [Link]
sortLinks urls users medias = sortBy sortDesc (map makeURLLink   urls ++
                                               map makeUserLink  users ++
                                               map makeMediaLink medias)
  where sortDesc a b
          | startIndex a < startIndex b = LT
          | otherwise = GT

makeURLLink :: URLEntity -> Link
makeURLLink urlEntity = Link x y url
  where x       = head (urlIndices urlEntity)
        y       = urlIndices urlEntity !! 1
        urlText = displayUrl urlEntity
        href    = urlMessage urlEntity
        url     = "<a target=\"_blank\" href=\"" ++ href ++ "\">"
                    ++ urlText ++ "</a>"

makeMediaLink :: MediaEntity -> Link
makeMediaLink mediaEntity = Link x y url
  where x       = head (mediaIndices mediaEntity)
        y       = mediaIndices mediaEntity !! 1
        urlText = displayMediaUrl mediaEntity
        href    = mediaUrl mediaEntity
        url     = "<a target=\"_blank\" href=\"" ++ href ++ "\">"
                    ++ urlText ++ "</a>"

makeUserLink :: UserEntity -> Link
makeUserLink userEntity = Link x y mention
  where x        = head (userIndices userEntity)
        y        = userIndices userEntity !! 1
        username = screenName userEntity
        mention  = "<a target=\"_blank\" href=\"//twitter.com/" ++ username
                   ++ "\">@" ++ username ++ "</a>"

simplifyTweet :: Tweet -> SimpleTweet
simplifyTweet tweet =
    SimpleTweet { body = text tweet
                , tweetId = idStr tweet
                , created_at = createdAt tweet }

addLink :: Link -> String -> String
addLink link tweet = before ++ newHtml link ++ after
  where before = fst (splitAt (startIndex link) tweet)
        after  = snd (splitAt (endIndex link) tweet)
