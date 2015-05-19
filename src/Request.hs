{-# LANGUAGE RankNTypes #-}

module Request where

-- Imports

import qualified RAML               as R
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.List.Split    as S
import qualified Network.Wreq       as W
import qualified Network.Wreq.Types as WT

import Data.Maybe
import Data.Monoid

-- Internal Data Structures

type RequestValidationError = String

type ReqVal a = Either RequestValidationError a

type TextLookup = [(T.Text, T.Text)]

-- Implementation

contextError :: forall b. String -> Either RequestValidationError b
contextError context = Left $ "Could not locate requested route: " ++ context

getUrlSegments :: String -> [R.PathSegment] -> R.RouteLookup -> ReqVal R.RouteInfo
getUrlSegments context [ ]    _ = contextError context
getUrlSegments context [x]    l = maybe (contextError context) Right $ M.lookup x l
getUrlSegments context (x:xs) l = do
  pre <- maybe (contextError context) Right $ M.lookup x l
  getUrlSegments context xs (R.subRoutes pre)

getUrl :: String -> R.RamlFile -> ReqVal R.RouteInfo
getUrl url raml = getUrlSegments url urlSegments routes
  where
  routes      = R.routes raml
  urlSegments = S.splitOn "/" url

getIncludedTraits :: R.RamlFile -> R.RouteInfo -> R.Info
getIncludedTraits raml routeInfo = mconcat relevant
  where
  traits   = R.traits raml
  included = R.traitList $ R.includedTraits routeInfo
  relevant = M.elems $ M.filterWithKey (\k _ -> elem k included) traits

methodError :: forall b. String -> ReqVal b
methodError context = Left $ "Could not locate requested method: " ++ context

getMethod :: R.Method -> R.RouteInfo -> ReqVal (Maybe R.Info)
getMethod m r = maybe (methodError (show m)) Right $ M.lookup m (R.methods r)

validateRequestSchema :: WT.Putable a => Maybe R.Schema -> Maybe a -> ReqVal ()
validateRequestSchema = undefined

validateQueryParams :: R.QueryLookup -> TextLookup -> ReqVal ()
validateQueryParams iParams oParams = undefined

validateRequest :: WT.Putable a => R.RamlFile
                                -> R.Method
                                -> W.Options
                                -> String
                                -> Maybe a
                                -> Either RequestValidationError ()
validateRequest raml verb options url body = do
  routeInfo    <- getUrl url raml
  info         <- fromMaybe mempty `fmap` getMethod verb routeInfo

  let included  = getIncludedTraits raml routeInfo
      infos     = info <> included

  validateRequestSchema (R.requestSchema infos) body
  validateQueryParams   (R.parameters    infos) (WT.params options)
