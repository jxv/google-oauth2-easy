{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Google.Authentication.Easy
  ( Service(..)
  , callPostTokenInfo'
  , callPostToken'
  , AccessTokenForm(..)
  , PostTokenBody(..)
  , PostTokenInfoResponse(..)
  , RefreshTokenResp(..)
  , RefreshTokenBody(..)
  , AuthorizationCodeResp(..)
  , AuthorizationCodeBody(..)
  , ServantError
  , Manager
  ) where

import qualified Data.HashMap.Lazy as HML (singleton, fromList)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson
import Data.Text
import Data.Text.Conversions
import Data.Proxy
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import Web.FormUrlEncoded (Form(..), ToForm(..))

apiBaseUrl :: BaseUrl
apiBaseUrl = BaseUrl Https "www.googleapis.com" 443 ""

type Api = "oauth2" :> (("v3" :> PostTokenInfo) :<|> ("v4" :> PostToken))

type PostTokenInfo = "tokeninfo" :> ReqBody '[FormUrlEncoded] AccessTokenForm :> Post '[JSON] PostTokenInfoResponse
type PostToken = "token" :> ReqBody '[FormUrlEncoded] PostTokenBody :> Post '[JSON] PostTokenResponse

data AccessTokenForm = AccessTokenForm
  { access_token :: Text
  } deriving (Show, Eq)

instance ToForm AccessTokenForm where
  toForm form = Form $ HML.singleton "access_token" [toText $ (access_token :: AccessTokenForm -> Text) form]

data PostTokenInfoResponse = PostTokenInfoResponse
  { azp :: Text
  , aud :: Text
  , sub :: Text
  , scope :: Text
  , exp :: Text
  , expires_in :: Text
  , email :: Text
  , email_verified :: Text
  , access_type :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON PostTokenInfoResponse

data PostTokenBody
  = PostTokenBody'AuthorizationCodeBody AuthorizationCodeBody
  | PostTokenBody'RefreshTokenBody RefreshTokenBody
  deriving (Show, Eq)

instance ToForm PostTokenBody where
  toForm (PostTokenBody'AuthorizationCodeBody body) = toForm body
  toForm (PostTokenBody'RefreshTokenBody body) = toForm body

data AuthorizationCodeBody = AuthorizationCodeBody
  { code :: Text
  , client_id :: Text
  , client_secret :: Text
  , redirect_uri :: Text -- "https://www.camp47.com"
  , grant_type :: Text -- "authorization_code"
  } deriving (Show, Eq, Generic)

instance ToJSON AuthorizationCodeBody

instance ToForm AuthorizationCodeBody where
  toForm AuthorizationCodeBody{code,client_id,client_secret,redirect_uri,grant_type} = Form $ HML.fromList
    [ ("code", [toText code])
    , ("client_id", [toText client_id])
    , ("client_secret", [toText client_secret])
    , ("redirect_uri", [toText redirect_uri])
    , ("grant_type", [toText grant_type])
    ]

data RefreshTokenBody = RefreshTokenBody
  { refresh_token :: Text
  , client_id :: Text
  , client_secret :: Text
  , grant_type :: Text -- "refresh_token"
  } deriving (Show, Eq, Generic)

instance ToForm RefreshTokenBody where
  toForm RefreshTokenBody{refresh_token,client_id,client_secret,grant_type} = Form $ HML.fromList
    [ ("refresh_token", [toText refresh_token])
    , ("client_id", [toText client_id])
    , ("client_secret", [toText client_secret])
    , ("grant_type", [toText grant_type])
    ]

data PostTokenResponse
  = PostTokenResponse'AuthorizationCodeResp AuthorizationCodeResp
  | PostTokenResponse'RefreshTokenResp RefreshTokenResp
  deriving (Show, Eq)

instance FromJSON PostTokenResponse where
  parseJSON v =
    (PostTokenResponse'AuthorizationCodeResp <$> parseJSON v) <|>
    (PostTokenResponse'RefreshTokenResp <$> parseJSON v)

data AuthorizationCodeResp = AuthorizationCodeResp
  { access_token :: Text
  , expires_in :: Int
  , token_type :: Text
  , refresh_token :: Text
  , id_token :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON AuthorizationCodeResp

data RefreshTokenResp = RefreshTokenResp
  { access_token :: Text
  , expires_in :: Int
  , token_type :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON RefreshTokenResp

api :: Proxy Api
api = Proxy

postTokenInfo :: AccessTokenForm -> ClientM PostTokenInfoResponse
postToken :: PostTokenBody -> ClientM PostTokenResponse
postTokenInfo :<|> postToken = client api

class Monad m => Service m where
  callPostTokenInfo :: AccessTokenForm -> m (Either ServantError PostTokenInfoResponse)
  callPostToken :: PostTokenBody -> m (Either ServantError PostTokenResponse)


callPostTokenInfo' :: (MonadIO m, MonadReader r m) => (r -> Manager) -> AccessTokenForm -> m (Either ServantError PostTokenInfoResponse)
callPostTokenInfo' getManager accessTokenForm = do
  manager <- asks getManager
  liftIO $ runClientM (postTokenInfo accessTokenForm) (ClientEnv manager apiBaseUrl)

callPostToken' :: (MonadIO m, MonadReader r m) => (r -> Manager) -> PostTokenBody -> m (Either ServantError PostTokenResponse)
callPostToken' getManager authorizationCodeForm = do
  manager <- asks getManager
  liftIO $ runClientM (postToken authorizationCodeForm) (ClientEnv manager apiBaseUrl)


