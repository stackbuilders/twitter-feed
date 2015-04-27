{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

---------------------------------------------------------
-- |
-- Module     : Web.Twitter.Types
-- License    : MIT (see the file LICENSE)
-- Maintainer : Justin Leitgeb <justin@stackbuilders.com>
--
-- Types for working with the twitter API.
---------------------------------------------------------
module Web.Twitter.Types
  ( Tweet(..)
  , SimpleTweet(..)
  , Link(..)
  , Entities(..)
  , URLEntity(..)
  , UserEntity(..)
  , MediaEntity(..)
  ) where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.Ord (comparing)
import GHC.Generics (Generic)

-- | Represents tweet start and end index
type BoundingIndices = (Int, Int)

-- | Represents a tweet as HTML
data SimpleTweet = SimpleTweet
  { body       :: String -- ^ Tweet content as HTML
  , tweetId    :: String -- ^ Tweet id
  , created_at :: String -- ^ Tweet created at date
  } deriving (Show, Generic)

-- | Represent a tweet as plain text
data Tweet =  Tweet
  { text       :: String    -- ^ Tweet content as plain text
  , createdAt  :: String    -- ^ Tweet created at date
  , idStr      :: String    -- ^ Tweet id
  , entities   :: Entities  -- ^ All references mentioned on the tweet
  } deriving (Show, Generic)

-- | Represent a list of all tweet references
data Entities = Entities
  { userEntities  :: [UserEntity]  -- ^ List of all user references mentioned on the tweet
  , urlEntities   :: [URLEntity]   -- ^ List of all urls references mentioned on the tweet
  , mediaEntities :: [MediaEntity] -- ^ List of all media references mentioned on the tweet
  } deriving (Show, Generic)

-- | Represent a user reference
data UserEntity = UserEntity
  { screenName  :: String          -- ^ User twitter name
  , userIndices :: BoundingIndices -- ^ Tweet start and end index
  } deriving (Show, Generic)

-- | Represent a url reference
data URLEntity = URLEntity
  { urlMessage   :: String          -- ^ Reference title
  , urlIndices   :: BoundingIndices -- ^ Tweet start and end index
  , displayUrl   :: String          -- ^ Reference url
  } deriving (Show, Generic)

-- | Represent a media reference
data MediaEntity = MediaEntity
  { mediaUrl        :: String          -- ^ Reference url
  , mediaIndices    :: BoundingIndices -- ^ Tweet start and end index
  , displayMediaUrl :: String          -- ^ Reference title
  } deriving (Show, Generic)

-- | Represent a link as HTML
data Link = Link
  { startIndex :: Int    -- ^ Tweet start index
  , endIndex   :: Int    -- ^ Tweet end index
  , newHtml    :: String -- ^ Link as HTML
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
