{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Redmine.TimeEntries.API
  ( API
  , proxy
  ) where


import           Data.Proxy                        (Proxy (..))
import qualified Data.Text                         as T
import qualified Redmine.TimeEntries.Activities    as TimeEntries
import qualified Redmine.TimeEntries.LimitedResult as TimeEntries
import qualified Redmine.TimeEntries.NewTimeEntry  as TimeEntries
import           Servant.API



-- API


type API =
    "time_entries.json"
      :> QueryParam' '[Required] "key" T.Text
      :> QueryParam' '[Required] "spent_on" T.Text
      :> QueryParam' '[Required] "user_id" Int
      :> QueryParam "sort" T.Text
      :> Get '[JSON] TimeEntries.LimitedResult
    :<|> "time_entries.json"
      :> QueryParam' '[Required] "key" T.Text
      :> ReqBody '[JSON] TimeEntries.NewTimeEntry
      :> Post '[JSON] NoContent
    :<|> "enumerations"
      :> "time_entry_activities.json"
      :> QueryParam' '[Required] "key" T.Text
      :> Get '[JSON] TimeEntries.Activities



-- PROXY


proxy :: Proxy API
proxy =
  Proxy
