{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.TimeEntry
  ( TimeEntry (..)
  , DisplayOptions (..)
  , display
  , createOptions
  , totalHours
  ) where


import           Control.Applicative
import           Data.Aeson
import qualified Data.Named          as Named
import qualified Data.Text           as T
import           GHC.Generics
import           Helper              ((|>))
import qualified System.Console.ANSI as ANSI



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


createOptions :: [TimeEntry] -> DisplayOptions
createOptions entries =
  let
    maxi [] = 0
    maxi a  = maximum a
  in
  DisplayOptions
    { maxProjectLength = maxi $ fmap projectLength entries
    , maxHoursLength =  maxi $ fmap hoursLength entries
    }


display :: DisplayOptions -> TimeEntry -> [(ANSI.SGR, T.Text)]
display options entry =
  let
    projectIndent =
      maxProjectLength options - projectLength entry
        |> indent

    hourIndent =
      maxHoursLength options - hoursLength entry
        |> indent
  in
    [ (blue, maybe (hourIndent <> "    ") (\d -> T.pack ("#" <> show d)) (issue entry))
    , (yellow, hourIndent <> T.pack (show (hours entry)))
    , (reset, Named.name (project entry) <> projectIndent)
    , (reset, comments entry)
    ]


totalHours :: [TimeEntry] -> Double
totalHours =
  sum . fmap hours



-- COLORS


blue :: ANSI.SGR
blue =
  ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue


yellow :: ANSI.SGR
yellow =
  ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow


reset :: ANSI.SGR
reset =
  ANSI.Reset



-- PRIVATE HELPERS


indent :: Int -> T.Text
indent i =
  T.replicate i " "


projectLength :: TimeEntry -> Int
projectLength =
  T.length . Named.name . project


hoursLength :: TimeEntry -> Int
hoursLength =
  length . show . hours



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
