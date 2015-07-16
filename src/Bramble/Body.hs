{-# LANGUAGE RankNTypes #-}

module Bramble.Body where

import qualified Data.Yaml as Y

class Validatable a where
  validate :: b -> a -> Either String ()

instance Validatable Y.Value where
  validate _ _ = Right ()
  -- TODO: Fill in the blanks
