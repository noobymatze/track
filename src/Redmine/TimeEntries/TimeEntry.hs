{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.TimeEntry
  ( TimeEntry (..)
  , DisplayOptions (..)
  , display
  , projectLength
  , hoursLength
  ) where


import           Control.Applicative
import           Data.Aeson
import qualified Data.Named          as Named
import qualified Data.Text           as T
import           GHC.Generics
import           Helper              ((|>))



-- TIME ENTRY


data TimeEntry
  = TimeEntry
  { id       :: Int
  , user     :: Named.Named
  , project  :: Named.Named
  , issue    :: Maybe Int
  , hours    :: Double
  , comments :: T.Text
  } deriving (Generic)



-- PUBLIC HELPERS


data DisplayOptions
  = DisplayOptions
    { maxProjectLength :: Int
    , maxHoursLength   :: Int
    } deriving (Show)


projectLength :: TimeEntry -> Int
projectLength =
  T.length . Named.name . project


hoursLength :: TimeEntry -> Int
hoursLength =
  length . show . hours


display :: DisplayOptions -> TimeEntry -> T.Text
display options entry =
  let
    projectIndent =
      maxProjectLength options - projectLength entry
        |> indent

    hourIndent =
      maxHoursLength options - hoursLength entry
        |> indent
  in
    T.intercalate " "
    [ maybe (hourIndent <> "    ") (\d -> T.pack ("#" <> show d)) (issue entry)
    , hourIndent <> T.pack (show (hours entry))
    , Named.name (project entry) <> projectIndent
    , comments entry
    ]



-- HELPERS


indent :: Int -> T.Text
indent i =
  T.replicate i " "



-- SERIALIZATION


instance FromJSON TimeEntry where
  parseJSON = withObject "TimeEntry" $ \v ->
    TimeEntry
      <$> v .: "id"
      <*> v .: "user"
      <*> v .: "project"
      <*> ((v .: "issue" >>= withObject "Issue" (.: "id")) <|> pure Nothing)
      <*> v .: "hours"
      <*> v .: "comments"
