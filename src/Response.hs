
module Response where

import qualified RAML                       as R
import qualified Network.Wreq               as W
import qualified Data.ByteString.Lazy.Char8 as B

data ResponseValidationError = ResVE deriving (Eq, Ord, Show)

validateResponse :: R.RamlFile
                 -> String
                 -> W.Options
                 -> String
                 -> W.Response B.ByteString
                 -> Either ResponseValidationError ()
validateResponse _ _ _ _ _ = return ()
