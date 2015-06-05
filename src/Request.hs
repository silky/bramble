{-# LANGUAGE RankNTypes #-}

module Request where

-- Imports

import qualified RAML               as R
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.List.Split    as S
import qualified Network.Wreq       as W
import qualified Network.Wreq.Types as WT
import qualified Body               as B

import Data.Maybe
import Data.Monoid

-- Internal Data Structures

type RequestValidationError = String
type ReqVal a               = Either RequestValidationError a
type TextLookup             = [(T.Text, T.Text)]

-- Implementation

contextError :: forall b. String -> Either RequestValidationError b
contextError context = Left $ "Could not locate requested route: " ++ context ++ " in spec."

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
methodError context = Left $ "Could not locate requested method: " ++ context ++ " in spec."

getMethod :: R.Method -> R.RouteInfo -> ReqVal (Maybe R.Info)
getMethod m r = maybe (methodError (show m)) Right $ M.lookup m (R.methods r)

validateRequestSchema :: B.Validatable a => Maybe R.Schema -> Maybe a -> ReqVal ()
validateRequestSchema _      Nothing     = return ()
validateRequestSchema schema (Just body) = B.validate schema body

validateQueryParams :: R.QueryLookup -> TextLookup -> ReqVal ()
validateQueryParams iParams oParams = mapM_ verifyParam oParams
  where
  verifyParam (k,_) = maybe (Left $ "Requested parameter " ++ show k ++ ", not in schema")
                            (const (return ()))
                            (M.lookup (T.unpack k) iParams) -- TODO: String/Text mismatch sucks

validateRequest :: B.Validatable a
                => R.RamlFile -> R.Method -> W.Options -> String -> Maybe a
                -> Either RequestValidationError ()
validateRequest raml verb options url body = do
  routeInfo    <- getUrl url raml
  info         <- fromMaybe mempty `fmap` getMethod verb routeInfo

  let included  = getIncludedTraits raml routeInfo
      infos     = info <> included

  validateRequestSchema (R.requestSchema infos) body
  validateQueryParams   (R.parameters    infos) (WT.params options)
