{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Prompt
  ( Prompt
  , yesOrNo
  , string
  , double
  , required
  , required'
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


required' :: T.Text -> Prompt (Maybe a) -> Prompt a
required' messageOnMaybe =
  required (Just messageOnMaybe)


required :: Maybe T.Text -> Prompt (Maybe a) -> Prompt a
required messageOnMaybe prompt =
  prompt >>= \maybeValue ->
    case maybeValue of
      Nothing ->
        case messageOnMaybe of
          Nothing -> do
            required messageOnMaybe prompt

          Just message -> do
            liftIO $ putStr (T.unpack message)
            required messageOnMaybe prompt


      Just value ->
        pure value


yesOrNo :: Prompt (Maybe Bool)
yesOrNo = readLine >>= \input ->
  if input `elem` ["Y", "y"] then
    pure (Just True)
  else if input `elem` ["N", "n"] then
    pure (Just False)
  else
    pure Nothing


string :: Prompt (Maybe T.Text)
string = T.strip <$> readLine >>= \input ->
  case input of
    "" ->
      pure Nothing

    value ->
      pure (Just value)


double :: Prompt (Maybe Double)
double = T.strip <$> readLine >>= \input ->
  case input of
    "" ->
      pure Nothing

    value ->
      pure (T.readMaybe (T.unpack value))


int :: Prompt (Maybe Int)
int = T.strip <$> readLine >>= \input ->
  case input of
    "" ->
      pure Nothing

    value ->
      pure (T.readMaybe (T.unpack value))



-- PRIVATE HELPERS


readLine :: Prompt T.Text
readLine =
  T.pack <$> liftIO getLine
