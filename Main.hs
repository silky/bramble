{- TODO:
 -
 - * Include Json-Schema support for schema validation
 -}

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Internal Imports

import qualified Bramble.IO as I

-- External Imports

import Text.Groom
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Either hiding (left)

-- Qualified Imports

import qualified Network.Wreq               as W
import qualified Data.Yaml                  as Y
import qualified Data.Aeson                 as J

-- Main

main :: IO ()
main = putStrLn . groom =<< mainE

myOptions :: W.Options
myOptions = W.defaults &~ W.checkStatus .= Just (\_ _ _ -> Nothing)

mainE :: IO (Either I.ValidationError J.Value)
mainE = runEitherT $ do
  schema    <- I.loadSchema "test/examples/worldmusic.raml"
  _         <- liftIO $ putStrLn $ groom schema
  apiResult <- I.client "GET" myOptions "http://httpbin.org/songs" (Just $ Y.toJSON [1 :: Int ,2,3]) schema
  res       <- I.getBody apiResult
  return res
