{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Prompt
  ( Prompt
  , yesOrNo
  , string
  , double
  , required
  , required_
  , label
  , run
  , int
  ) where


import           Control.Monad.IO.Class
import qualified Data.Text              as T
import qualified Text.Read              as T



-- PROMPT


newtype Prompt a
  = Prompt (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)



-- RUNNING PROMPT


run :: Prompt a -> IO a
run (Prompt io) =
  io



-- CONSTRUCTING PROMPTS


label :: T.Text -> Prompt a -> Prompt a
label l prompt = do
  liftIO $ putStr (T.unpack l)
  prompt


required :: T.Text -> Prompt (Maybe a) -> Prompt a
required =
  required_ . Just


required_ :: Maybe T.Text -> Prompt (Maybe a) -> Prompt a
required_ messageOnMaybe prompt = do
  maybeValue <- prompt
  case maybeValue of
    Nothing ->
      case messageOnMaybe of
        Nothing ->
          required_ messageOnMaybe prompt

        Just message -> do
          liftIO $ putStr (T.unpack message)
          required_ messageOnMaybe prompt

    Just value ->
      pure value



-- PRIMITIVE PROMPTS


yesOrNo :: Prompt (Maybe Bool)
yesOrNo = custom $ \input ->
  if input `elem` ["Y", "y"] then
    Just True
  else if input `elem` ["N", "n"] then
    Just False
  else
    Nothing


string :: Prompt (Maybe T.Text)
string =
  custom Just


double :: Prompt (Maybe Double)
double =
  custom (T.readMaybe . T.unpack)


int :: Prompt (Maybe Int)
int =
  custom (T.readMaybe . T.unpack)


custom :: (T.Text -> Maybe a) -> Prompt (Maybe a)
custom f = T.strip <$> readLine >>= \input ->
  case input of
    "" ->
      pure Nothing

    value ->
      pure (f value)



-- PRIVATE HELPERS


readLine :: Prompt T.Text
readLine =
  T.pack <$> liftIO getLine
