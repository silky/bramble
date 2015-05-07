{- TODO:
 -
 - * Include Json-Schema support for schema validation
 -}

module Main where

-- Internal Imports

import qualified RAML as R

-- External Imports

import Text.Groom
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.Either hiding (left)

-- Qualified Imports

import qualified Network.Wreq               as W
import qualified Network.Wreq.Types         as W
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Yaml.Include          as Y
import qualified Data.Yaml                  as YE

-- Main

mainE :: IO (Either ValidationError (W.Response B.ByteString))
mainE = runEitherT $ do
  loadedRaml <- liftIO $ Y.decodeFileEither "resources/worldmusic.raml"
  _          <- liftIO $ putStrLn $ groom loadedRaml -- TODO: Debugging
  ramlE      <- fromEither $ left ParseError loadedRaml
  apiResult  <- liftIO $ genericClientSchema ramlE "PATCH" W.defaults
                  "http://httpbin.org/patch"
                  (Just $ YE.toJSON [1 :: Int ,2,3])
  fromEither apiResult

main :: IO ()
main = putStrLn . groom =<< mainE

-- Implementation

genericClient :: W.Putable a => String -> W.Options -> String -> Maybe a -> IO (W.Response B.ByteString)
genericClient verb options url body = W.customMethodPayloadMaybeWith verb options url body

data RequestValidationError  = ReqVE deriving (Eq, Ord, Show)
data ResponseValidationError = ResVE deriving (Eq, Ord, Show)
data ValidationError         = ParseError YE.ParseException
                             | RequestValidationError | ResponseValidationError
                               deriving (Show)

fromEither :: Either l a -> EitherT l IO a
fromEither = EitherT . return

validateRequest :: W.Putable a
                   => R.RamlFile -> String -> W.Options -> String -> Maybe a
                   -> Either ValidationError ()
validateRequest _ _ _ _ _ = return ()

validateResponse :: R.RamlFile -> String -> W.Options -> String
                 -> W.Response B.ByteString -> Either ValidationError ()
validateResponse _ _ _ _ _ = return ()

genericClientSchema :: W.Putable a
                    => R.RamlFile -> String -> W.Options -> String -> Maybe a
                    -> IO (Either ValidationError (W.Response B.ByteString))
genericClientSchema raml verb options url body = runEitherT $ do
  _        <- fromEither $ validateRequest raml verb options url body
  response <- liftIO $ W.customMethodPayloadMaybeWith verb options url body
  _        <- fromEither $ validateResponse raml verb options url response
  return response
