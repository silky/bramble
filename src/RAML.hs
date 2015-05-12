
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}

module RAML where

-- External qualified imports:

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Types           as J

-- STDLIB qualified imports:

import qualified Data.Map                   as M
import qualified Data.HashMap.Strict        as H
import qualified Data.Vector                as V
import qualified Data.Text                  as T
import qualified Data.List                  as L

-- Unqualified imports:

import Data.Aeson ((.:?), (.!=))
import Data.Monoid
import Control.Arrow
import Safe

-- Simple type aliases:
-- TODO: Wrap these in newtypes

type MIME         = String
type PathSegment  = String
type TraitName    = String
type ParamName    = String
type ResponseCode = Int

-- Helper function to perform pairwise monadic actions - "Half-Kleisli"?

(/*/) :: Monad m => (t -> m t2) -> (t1 -> m t3) -> (t, t1) -> m (t2, t3)
a /*/ b = runKleisli (Kleisli a *** Kleisli b)
infix 4 /*/

(*/) :: Monad m => (t -> t2) -> (t1 -> m t3) -> (t, t1) -> m (t2, t3)
a */ b = (return . a /*/ b)
infix 4 */

(/*) :: Monad m => (t -> m t2) -> (t1 -> t3) -> (t, t1) -> m (t2, t3)
a /* b = (a /*/ return . b)
infix 4 /*

-- Convert value to a response code

intKey :: T.Text -> J.Parser ResponseCode
intKey k = maybe (k <~> "intKey") return $ readMay (T.unpack k)

-- Fail with a helpful message
(<~>) :: (Show a, Monad m) => a -> [Char] -> m a1
v <~> s = fail $ "Found unexpected value during " ++ s ++ " ~> " ++ show v

instance J.FromJSON ResponseLookup where
  parseJSON (J.Object o) = fmap M.fromList $ mapM (intKey /*/ J.parseJSON) (H.toList o)
  parseJSON x            = x <~> "ResponseLookup"

-- Lookups:

type Lookup a b     = M.Map  a            b
type RouteLookup    = Lookup PathSegment  RouteInfo
type MimeLookup     = Lookup MIME         ResponseInfo
type ResponseLookup = Lookup ResponseCode ResponseComponents
type MethodLookup   = Lookup Method       (Maybe Info)
type TraitLookup    = Lookup TraitName    Info
type QueryLookup    = Lookup ParamName    ParamSpec

emptyLookup :: forall k a. M.Map k a
emptyLookup = M.empty

-- Data Types and Instances:

data RamlFile = RamlFile
  { metaData :: Metadata
  , traits   :: TraitLookup
  , routes   :: RouteLookup } deriving (Eq,Ord,Show)

instance J.FromJSON RamlFile where
  parseJSON (J.Object o) = do
    traits   <- getTraits   o
    metaData <- getMetadata o
    routes   <- getRoutes   o
    return RamlFile { .. }

  parseJSON x = x <~> "RamlFile"

data Metadata = Metadata
  { ramlVersion :: Maybe String
  , title       :: Maybe String
  , baseUri     :: Maybe String
  , version     :: Maybe String } deriving (Eq,Ord,Show)

data Method = Get | Put | Post | Delete | Custom String deriving (Eq,Ord,Show)

data ParamType = StringParam | NumberParam  | IntegerParam
               | DateParam   | BooleanParam | FileParam deriving (Eq,Ord,Show)

stringToParam :: T.Text -> J.Parser ParamType
stringToParam "string"  = return StringParam
stringToParam "number"  = return NumberParam
stringToParam "integer" = return IntegerParam
stringToParam "date"    = return DateParam
stringToParam "boolean" = return BooleanParam
stringToParam "file"    = return FileParam
stringToParam x         = x <~> "StringToParam"

instance J.FromJSON ParamType where
  parseJSON (J.String s) = stringToParam s
  parseJSON x            = x <~> "ParamType"

data RouteInfo = RouteInfo
  { methods        :: MethodLookup
  , includedTraits :: TraitList
  , subRoutes      :: RouteLookup } deriving (Eq,Ord,Show)

isRoute :: (T.Text, t) -> Bool
isRoute (k,_) = T.isPrefixOf "/" k

textToMethod :: Monad m => T.Text -> m Method
textToMethod "get"    = return Get
textToMethod "put"    = return Put
textToMethod "post"   = return Post
textToMethod "delete" = return Delete
textToMethod x        = return $ Custom (T.unpack x)

stringToMethod :: Monad m => String -> m Method
stringToMethod = textToMethod . T.pack

getIncludedTraits :: J.Object -> J.Parser TraitList
getIncludedTraits o = o .:? "is" .!= emptyTraitList

instance J.FromJSON RouteInfo where
  parseJSON (J.Object o) = do
    let items               = H.toList o
        (pRoutes, mMethods) = L.partition isRoute items
        (_inc,    pMethods) = L.partition ((== "is") . fst) mMethods
    subRoutes              <- fmap M.fromList $ mapM (T.unpack      */ J.parseJSON) pRoutes
    methods                <- fmap M.fromList $ mapM (textToMethod /*/ J.parseJSON) pMethods
    includedTraits         <- getIncludedTraits o
    return RouteInfo { .. }

  parseJSON x = x <~> "RouteInfo"

data Info = Info
  { description    :: Maybe String
  , parameters     :: QueryLookup
  , requestSchema  :: Maybe Schema
  , responses      :: ResponseLookup } deriving (Eq,Ord,Show)

instance Monoid Info where
  mempty = Info { description    = Nothing
                , parameters     = M.empty
                , requestSchema  = Nothing
                , responses      = M.empty
                }

  mappend (Info { description    = aDesc
                , parameters     = aParams
                , requestSchema  = aReqSchema
                , responses      = aRes })
          (Info { description    = bDesc
                , parameters     = bParams
                , requestSchema  = bReqSchema
                , responses      = bRes })
         = Info { description    = combineDescriptions aDesc bDesc
                , parameters     = M.union aParams bParams
                , requestSchema  = combineSchemas aReqSchema bReqSchema
                , responses      = M.union aRes bRes }

    where

    combineDescriptions a        Nothing  = a
    combineDescriptions Nothing  b        = b
    combineDescriptions (Just a) (Just b) = Just (a ++ "; "++ b)

    combineSchemas      a        Nothing  = a -- TODO: Support combining schemas by validating both
    combineSchemas      Nothing  b        = b
    combineSchemas      _        _        = error "Combining two schemas isn't currently supported..."


getParameters :: J.Object -> J.Parser QueryLookup
getParameters o = o .:? "queryParameters" .!= emptyLookup

getResponses :: J.Object -> J.Parser ResponseLookup
getResponses o = o .:? "responses" .!= emptyLookup

instance J.FromJSON Info where
  parseJSON (J.Object o) = do
    description    <- o .:? "description"
    parameters     <- getParameters o
    requestSchema  <- o .:? "schema"
    responses      <- getResponses o
    return Info { .. }

  parseJSON x = x <~> "Info"

data ResponseComponents = ResponseComponents
  { bodyTypes :: MimeLookup } deriving (Eq, Ord, Show)

instance J.FromJSON ResponseComponents where
  parseJSON (J.Object o) = do
    bodyTypes <- o .:? "body" .!= emptyLookup
    return ResponseComponents { .. }

  parseJSON x = x <~> "ResponseComponents"

data ParamSpec = ParamSpec
  { paramDescription :: Maybe String
  , displayName      :: Maybe String
  , paramType        :: Maybe ParamType } deriving (Eq,Ord,Show)

instance J.FromJSON ParamSpec where
  parseJSON (J.Object o) = do
    paramDescription <- o .:? "description"
    displayName      <- o .:? "displayName"
    paramType        <- o .:? "type"
    return ParamSpec { .. }

  parseJSON x = x <~> "ParamSpec"

data ResponseInfo = ResponseInfo
  { example :: Maybe String
  , schema  :: Maybe Schema } deriving (Eq,Ord,Show)

instance J.FromJSON ResponseInfo where
  parseJSON (J.Object o) = do
    example <- o .:? "example"
    schema  <- o .:? "schema"
    return ResponseInfo { .. }

  parseJSON J.Null = return $ ResponseInfo Nothing Nothing

  parseJSON x = x <~> "ResponseInfo"

newtype Schema = Schema { getSchemaText :: T.Text } deriving (Eq,Ord)

-- TODO: Show instance hack for groom output only
instance Show Schema where
  show = ("Schema " ++) . show . (++ "...") . T.unpack . T.take 50 . getSchemaText

instance J.FromJSON Schema where -- TODO: Use a real schema object
  parseJSON (J.String s) = return $ Schema s
  parseJSON x            = x <~> "Schema"

newtype TraitList = TraitList
  { traitList :: [TraitName] } deriving (Eq, Ord, Show, Monoid)

emptyTraitList :: TraitList
emptyTraitList = TraitList []

instance J.FromJSON TraitList where
  parseJSON (J.Array a) = do
    traitList <- mapM J.parseJSON (V.toList a)
    return TraitList { .. }

  parseJSON x = x <~> "TraitList"

-- Helper Functions:

emptyObject :: J.Value
emptyObject = J.Object H.empty

getTraits :: J.Object -> J.Parser TraitLookup
getTraits o = do
  pTraits <- o .:? "traits" .!= emptyObject
  lTraits <- J.parseJSON pTraits
  return   $ M.unions lTraits

getRoutes :: J.Object -> J.Parser RouteLookup
getRoutes o = J.parseJSON (J.Object routeList)
  where
  routeList = H.filterWithKey kTest o
  kTest k _ = T.head k == '/'

getMetadata :: J.Object -> J.Parser Metadata
getMetadata o = do
  let ramlVersion = Nothing -- Seemingly no way to extract this through regular YAML parsing
  title   <- o .:? "title"
  baseUri <- o .:? "baseUri"
  version <- o .:? "version"
  return Metadata { .. }
