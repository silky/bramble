
module Request where

import qualified RAML               as R
import qualified Network.Wreq       as W
import qualified Network.Wreq.Types as W

data RequestValidationError = ReqVE deriving (Eq, Ord, Show)

validateRequest :: W.Putable a
                => R.RamlFile
                -> String
                -> W.Options
                -> String
                -> Maybe a
                -> Either RequestValidationError ()
validateRequest _ _ _ _ _ = return ()
