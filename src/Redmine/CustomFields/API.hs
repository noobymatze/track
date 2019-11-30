{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Redmine.CustomFields.API
  ( API
  , proxy
  ) where


import           Data.Proxy                          (Proxy (..))
import qualified Data.Text                           as T
import qualified Redmine.CustomFields.CustomFieldRsp as CustomFields
import           Servant.API



-- CUSTOM FIELDS API


type API =
    "custom_fields.json"
      :> QueryParam' '[Required] "key" T.Text
      :> Get '[JSON] CustomFields.CustomFieldRsp



-- CONSTRUCT API


proxy :: Proxy API
proxy =
  Proxy
