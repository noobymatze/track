module Redmine.CustomFields.Client
  ( getAll
  ) where


import qualified App
import qualified Data.Text                           as T
import qualified Redmine.CustomFields.API            as API
import qualified Redmine.CustomFields.CustomField    as CustomField
import qualified Redmine.CustomFields.CustomFieldRsp as CustomFields



-- GET ALL


getAll :: App.App [CustomField.CustomField]
getAll =
  CustomFields.custom_fields <$> (App.getApiKey >>= getAll_)


getAll_ :: T.Text -> App.App CustomFields.CustomFieldRsp
getAll_ = App.client API.proxy

