module Main where

import Bramble.IO
import Bramble.RAML
import Data.Either
import System.Exit
import System.Directory
import System.Environment
import Control.Monad.Trans.Either

main :: IO ()
main = do
  args    <- getArgs
  results <- case args of [] -> find "test/examples" >>= testFiles
                          xs -> testFiles xs
  putStrLn ""
  putStrLn $ "Results: " ++ show (length (rights results)) ++ "/" ++ show (length results) ++ " passed"
  putStrLn ""

  either (const exitFailure) (const exitSuccess) (sequence results)

find :: String -> IO [String]
find x = fmap (map (prefix x) . filter removeLinks) (getDirectoryContents x)
  where
  removeLinks = not . flip elem [".",".."]
  prefix d f  = d ++ "/" ++ f

testFiles :: [String] -> IO [Either ValidationError RamlFile]
testFiles = mapM test

rightPad :: String -> String -- rightPad a string up to length 55
rightPad x = x ++ replicate (max 0 (57 - length x)) ' '

test :: String -> IO (Either ValidationError RamlFile)
test x = do
  loadedRaml <- runEitherT $ loadSchema x
  case loadedRaml of (Left  e) -> putStrLn $ rightPad x ++ " -> " ++ show e
                     (Right _) -> putStrLn $ rightPad x ++ " -> " ++ "SUCCESS"
  return loadedRaml
