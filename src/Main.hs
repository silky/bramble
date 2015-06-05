{- TODO:
 -
 - * Include Json-Schema support for schema validation
 -}

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Internal Imports

import qualified RAML     as R
import qualified Request  as Req
import qualified Response as Res
import qualified Body     as B

-- External Imports

import Text.Groom
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.Either hiding (left)

-- Qualified Imports

import qualified Network.Wreq               as W
import qualified Network.Wreq.Types         as W
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Yaml.Include          as YI
import qualified Data.Yaml                  as Y

-- Main

main :: IO ()
main = putStrLn . groom =<< mainE

mainE :: IO (Either ValidationError (W.Response B.ByteString))
mainE = runEitherT $ do
  loadedRaml <- liftIO $ YI.decodeFileEither "resources/worldmusic.raml"
  _          <- liftIO $ putStrLn $ groom loadedRaml -- TODO: Debugging
  ramlE      <- fromEither $ left ParseError loadedRaml
  apiResult  <- liftIO $ genericClientSchema ramlE "GET" W.defaults
                  "http://httpbin.org/patch"
                  (Just $ Y.toJSON [1 :: Int ,2,3])
  fromEither apiResult

-- Implementation

data ValidationError = ParseError   Y.ParseException
                     | RequestError Req.RequestValidationError
                     | ResultError  Res.ResponseValidationError
                     deriving (Show)

fromEither :: Either l a -> EitherT l IO a
fromEither = EitherT . return

genericClientSchema :: (B.Validatable a, W.Putable a)
                    => R.RamlFile -> R.Method -> W.Options -> String -> Maybe a
                    -> IO (Either ValidationError (W.Response B.ByteString))
genericClientSchema raml verb options url body = runEitherT $ do
  _        <- fromEither $ left RequestError $ Req.validateRequest raml verb options url body
  response <- liftIO $ W.customMethodPayloadMaybeWith (show verb) options url body
  _        <- fromEither $ left ResultError $ Res.validateResponse raml verb options url response
  return response
