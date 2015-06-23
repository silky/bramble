module Main where

import IO
import RAML
import System.Exit
import System.Directory
import Control.Monad.Trans.Either

main :: IO ()
main = do
  results <- testFiles
  case sequence results of (Left  _) -> exitFailure
                           (Right _) -> exitSuccess

testFiles :: IO [Either ValidationError RamlFile]
testFiles = mapM test . filter removeLinks =<< getDirectoryContents "test/examples"
  where
  removeLinks = not . flip elem [".",".."]

test :: String -> IO (Either ValidationError RamlFile)
test x = do
  loadedRaml <- runEitherT $ loadSchema x
  case loadedRaml of (Left  _) -> print ("Failed to load    " ++ x)
                     (Right _) -> print ("Loading succeeded " ++ x)
  return loadedRaml
