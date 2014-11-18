{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Twitter.Types
  ( Tweet(..)
  , SimpleTweet(..)
  , Link(..)
  , Entities(..)
  , URLEntity(..)
  , UserEntity(..)
  , MediaEntity(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Ord (comparing)
import GHC.Generics (Generic)

type BoundingIndices = (Int, Int)

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

data Entities = Entities
  { userEntities  :: [UserEntity]
  , urlEntities   :: [URLEntity]
  , mediaEntities :: [MediaEntity]
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

data Link = Link
  { startIndex :: Int
  , endIndex   :: Int
  , newHtml    :: String
  } deriving (Show, Eq)

instance ToJSON SimpleTweet

instance ToJSON Tweet
instance FromJSON Tweet where
  parseJSON = withObject "Tweet" parseTweet

instance ToJSON Entities
instance FromJSON Entities where
  parseJSON = withObject "Entities" parseEntities

instance ToJSON UserEntity
instance FromJSON UserEntity where
  parseJSON = withObject "UserEntity" parseUserEntity

instance ToJSON URLEntity
instance FromJSON URLEntity where
  parseJSON = withObject "URLEntity" parseURLEntity

instance ToJSON MediaEntity
instance FromJSON MediaEntity where
  parseJSON  = withObject "MediaEntity" parseMediaEntity

instance Ord Link where
  compare = comparing startIndex

parseTweet :: Object -> Parser Tweet
parseTweet v = Tweet <$> v .: "text"
                     <*> v .: "created_at"
                     <*> v .: "id_str"
                     <*> v .: "entities"

parseUserEntity :: Object -> Parser UserEntity
parseUserEntity v = UserEntity <$> v .: "screen_name"
                               <*> v .: "indices"

parseURLEntity :: Object -> Parser URLEntity
parseURLEntity v = URLEntity <$> v .: "url"
                             <*> v .: "indices"
                             <*> v .: "display_url"

parseMediaEntity :: Object -> Parser MediaEntity
parseMediaEntity v = MediaEntity <$> v .: "url"
                                 <*> v .: "indices"
                                 <*> v .: "display_url"

parseEntities :: Object -> Parser Entities
parseEntities v = Entities <$> v .:? "user_mentions" .!= []
                           <*> v .:? "urls" .!= []
                           <*> v .:? "media" .!= []
