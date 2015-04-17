
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes        #-}

-- A little prototype for RAML client verification

module Prototype where

-- External qualified imports:

import qualified Data.Yaml.Include          as Y
import qualified Data.Aeson                 as J
import qualified Data.Aeson.Types           as J
import qualified Data.Aeson.Encode.Pretty   as J

-- STDLIB qualified imports:

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Exception          as E
import qualified Data.Map                   as M
import qualified Data.HashMap.Strict        as H
import qualified Data.Text                  as T

-- Unqualified imports:

import Data.Aeson ((.:?), (.!=))
import Control.Monad
import Debug.Trace

-- Simple type aliases:

type MIME         = String
type PathSegment  = String
type TraitName    = String
type ParamName    = String
type ResponseCode = Int

-- Lookups:

type Lookup a b     = M.Map  a            b
type RouteLookup    = Lookup PathSegment  RouteInfo
type MimeLookup     = Lookup MIME         BlockInfo
type ResponseLookup = Lookup ResponseCode ResponseInfo
type MethodLookup   = Lookup Method       Info
type TraitLookup    = Lookup TraitName    Info
type QueryLookup    = Lookup ParamName    ParamSpec

emptyLookup :: forall k a. M.Map k a
emptyLookup = M.empty

-- Data Types and Instances:

data RamlFile = RamlFile
  { metaData :: RamlMetadata
  , traits   :: TraitLookup
  , routes   :: RouteLookup } deriving (Eq,Ord,Show)

instance J.FromJSON RamlFile where
  parseJSON (J.Object o) = do
    traits   <- getTraits   o
    metaData <- getMetadata o
    routes   <- getRoutes   o
    return RamlFile { .. } -- Thanks -XRecordWildCards!

  parseJSON _ = mzero

data RamlMetadata = RamlMetadata
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
stringToParam _         = mzero

instance J.FromJSON ParamType where
  parseJSON (J.String s) = stringToParam s
  parseJSON _            = mzero

data RouteInfo = RouteInfo
  { methods   :: MethodLookup
  , subRoutes :: RouteLookup } deriving (Eq,Ord,Show)

instance J.FromJSON RouteInfo where
  parseJSON o = trace (show o) $ E.assert False undefined

data Info = Info
  { description    :: Maybe String
  , parameters     :: QueryLookup
  , requests       :: BlockInfo
  , requestSchema  :: Maybe Schema
  , responses      :: ResponseLookup
  , responseSchema :: Maybe Schema } deriving (Eq,Ord,Show)

getParameters :: J.Object -> J.Parser QueryLookup
getParameters o = do
  ps <- o .:? "queryParameters" .!= emptyLookup
  return ps

getRequests       = E.assert False undefined
getRequestSchema  = E.assert False undefined
getResponses      = E.assert False undefined
getResponseSchema = E.assert False undefined

instance J.FromJSON Info where
  parseJSON (J.Object o) = do
    description    <- o .:? "description"
    parameters     <- getParameters     o
    requests       <- getRequests       o
    requestSchema  <- getRequestSchema  o
    responses      <- getResponses      o
    responseSchema <- getResponseSchema o
    return Info { .. }

  parseJSON _ = mzero

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

  parseJSON _ = mzero

data ResponseInfo = ResponseInfo
  { responseBody :: MimeLookup } deriving (Eq,Ord,Show)

data BlockInfo = BlockInfo
  { example :: Maybe String
  , schema  :: Maybe String } deriving (Eq,Ord,Show)

instance J.FromJSON BlockInfo where
  parseJSON o = trace (show o) $ E.assert False undefined

data Schema = Schema String deriving (Eq,Ord,Show)

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

getMetadata :: J.Object -> J.Parser RamlMetadata
getMetadata o = do
  let ramlVersion = Nothing -- Seemingly no way to extract this through regular YAML parsing
  title   <- o .:? "title"
  baseUri <- o .:? "baseUri"
  version <- o .:? "version"
  return RamlMetadata { .. }

-- Parsers:

parseRaml :: String -> IO (Maybe RamlFile)
parseRaml = Y.decodeFile

parseValue :: String -> IO (Maybe J.Value)
parseValue = Y.decodeFile

showYaml :: Maybe J.Value -> IO ()
showYaml Nothing  = return ()
showYaml (Just x) = B.putStrLn $ J.encodePretty x

-- Load the RAML file and build a client.
-- Use the client to fetch a webpage,
-- validating both the request and the response.

main :: IO ()
main = do
  parseValue "resources/worldmusic.raml" >>= showYaml
  parseRaml  "resources/worldmusic.raml" >>= print
