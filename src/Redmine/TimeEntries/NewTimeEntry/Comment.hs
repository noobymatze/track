{-# LANGUAGE OverloadedStrings #-}
module Redmine.TimeEntries.NewTimeEntry.Comment
  ( Comment
  , display
  , parse
  , getTimeSpent
  , getIssue
  ) where

import           Data.Semigroup             ((<>))
import qualified Data.Text                  as T
import qualified Data.Time                  as Time
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as M
import qualified Text.Megaparsec.Char.Lexer as L



-- COMMENT


data Comment
  = Comment
    { maybeIssueId  :: Maybe Int
    , maybeInterval :: Maybe (Time.TimeOfDay, Time.TimeOfDay)
    , commentRest   :: T.Text
    } deriving (Show)



-- PUBLIC HELPERS


getIssue :: Comment -> Maybe Int
getIssue =
  maybeIssueId



getTimeSpent :: Comment -> Maybe Double
getTimeSpent comment = do
  (startTimeOfDay, endTimeOfDay) <- maybeInterval comment
  let start = Time.timeOfDayToTime startTimeOfDay
  let end = Time.timeOfDayToTime endTimeOfDay
  -- Converting it back to timeOfDay feels like a hack, but
  -- since we assume, that the times have to be on the same day
  -- anways, we can safely do this.
  let diff = Time.timeToTimeOfDay (end - start)
  pure (fromIntegral (Time.todHour diff) + toFractionOfHour (Time.todMin diff))


display :: Comment -> T.Text
display comment =
  T.intercalate ""
    [ case maybeIssueId comment of
        Nothing ->
          ""

        Just issue ->
          "[#" <> T.pack (show issue) <> "] "
    , case maybeInterval comment of
        Nothing ->
          ""

        Just (start, end) ->
          let
            format =
              T.pack . Time.formatTime Time.defaultTimeLocale "%H:%M"
          in
            format start <> " - " <> format end
    , commentRest comment
    ]



-- PARSER


type Parser = M.Parsec Void T.Text


parse :: T.Text -> Either (M.ParseErrorBundle T.Text Void) Comment
parse =
  M.parse parser ""


parser :: Parser Comment
parser =
  Comment
    <$> (M.space >> M.optional issueId)
    <*> (M.space >> M.optional timeDuration)
    <*> M.takeRest


timeDuration :: Parser (Time.TimeOfDay, Time.TimeOfDay)
timeDuration = do
  start <- timeOfDay
  _     <- M.space >> M.char '-' >> M.space
  end   <- timeOfDay
  pure (start, end)


timeOfDay :: Parser Time.TimeOfDay
timeOfDay = do
  hour   <- L.decimal <* M.char ':'
  minute <- L.decimal
  case Time.makeTimeOfDayValid hour minute 0 of
    Nothing ->
      fail $ T.unpack $ T.intercalate ""
        [ "Hour or minute of " <> T.pack (show hour)
        , ":" <> T.pack (show minute)
        , " are not valid hours and minutes of a day."
        ]

    Just value ->
      pure value


issueId :: Parser Int
issueId =
  M.string "[#" *> L.decimal <* M.string "]"




-- HELPERS


toFractionOfHour :: Int -> Double
toFractionOfHour i =
  fromIntegral (ceiling ((fromIntegral i :: Double) / 15.0) :: Integer) * 0.25
