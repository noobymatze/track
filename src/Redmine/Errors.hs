{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Redmine.Errors
  ( Errors (..)
  , display
  ) where


import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics



-- ERRORS


newtype Errors
  = Errors
    { errors :: [T.Text]
    } deriving (Show, Generic)


-- PUBLIC HELPERS


display :: Errors -> T.Text
display (Errors errors_) =
  T.intercalate "\n" $ map ("- "<>) errors_



-- SERIALIZATION


instance FromJSON Errors where
