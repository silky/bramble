
module Bramble.IO (

  -- Re-Exports
  module R,

  -- Data
  ValidationError (..),

  -- Functions
  loadGeneric,
  loadSchema,
  getBody,
  client

) where

-- TODO: Re-export RAML

import qualified Bramble.Body     as B
import qualified Bramble.RAML     as R
import qualified Bramble.Request  as Req
import qualified Bramble.Response as Res

import Control.Lens
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either hiding (left)

import qualified Network.Wreq               as W
import qualified Network.Wreq.Types         as W hiding (checkStatus, manager)
import qualified Data.Yaml.Include          as YI
import qualified Data.Yaml                  as Y
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Aeson                 as J

-- Data

data ValidationError = ParseError       Y.ParseException
                     | RequestError     Req.RequestValidationError
                     | ResultError      Res.ResponseValidationError
                     | ResultParseError String
                     deriving (Show)

-- Helpers

fromEither :: Either l a -> EitherT l IO a
fromEither = EitherT . return

loadGeneric :: Y.FromJSON json => FilePath -> EitherT Y.ParseException IO json
loadGeneric filePath = do
  loadedRaml <- liftIO     $ YI.decodeFileEither filePath
  ramlE      <- fromEither $ loadedRaml
  return ramlE

mapLeft :: Functor m => (a -> b) -> EitherT a m r -> EitherT b m r
mapLeft f e = EitherT $ left f <$> runEitherT e

-- Unified errors
--
loadSchema :: FilePath -> EitherT ValidationError IO R.RamlFile
loadSchema = mapLeft ParseError . loadGeneric

getBody :: (J.FromJSON r) => W.Response C8.ByteString -> EitherT ValidationError IO r
getBody response = do
  fromEither $ left ResultParseError $ J.eitherDecode $ view W.responseBody response

client :: (B.Validatable a, W.Putable a)
       => R.Method -> W.Options -> String -> Maybe a -> R.RamlFile
       -> EitherT ValidationError IO (W.Response C8.ByteString)
client verb options url body raml = do
  _         <- fromEither $ left RequestError $ Req.validateRequest raml verb options url body
  response  <- liftIO $ W.customMethodPayloadMaybeWith (show verb) options url body
  _         <- fromEither $ left ResultError $ Res.validateResponse raml verb options url response
  return response
