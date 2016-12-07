{-# LANGUAGE OverloadedStrings #-}

module Gcal.Parameter (makeParameter, Parameter (..)) where

import Data.Monoid ((<>))
import Data.List (foldl')
import Data.String (IsString)

data Parameter a =
  ClientID a
  | ClientSecret a
  | RefreshToken a
  | GrantType a
  | ResponseType a
  | RedirectURI a
  | Scope a
  | Code a
  | AccessToken a
  | Key a
  | SingleEvents a
  | OrderBy a
  | TimeMin a
  | TimeMax a deriving (Show, Eq)

toString :: (Monoid a, IsString a) => Parameter a -> a
toString (ClientID a)     = "client_id="     <> a
toString (ClientSecret a) = "client_secret=" <> a
toString (RefreshToken a) = "refresh_token=" <> a
toString (GrantType a)    = "grant_type="    <> a
toString (ResponseType a) = "response_type=" <> a
toString (RedirectURI a)  = "redirect_uri="  <> a
toString (Scope a)        = "scope="         <> a
toString (Code a)         = "code="          <> a
toString (AccessToken a)  = "access_token="  <> a
toString (Key a)          = "key="           <> a
toString (SingleEvents a) = "singleEvents="  <> a
toString (OrderBy a)      = "orderBy="       <> a
toString (TimeMin a)      = "timeMin="       <> a
toString (TimeMax a)      = "timeMax="       <> a

makeParameter :: (Monoid a, IsString a, Eq a) => [Parameter a] -> a
makeParameter = foldl' conc mempty
  where conc seed element
          | seed == "" = "?" <> toString element
          | otherwise  = seed <> "&" <> toString element
