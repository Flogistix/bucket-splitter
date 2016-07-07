module Main where

import Data.Maybe

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Network.AWS.Types (Region (..))
import Network.AWS.Data.Text

import Text.Read

import Data.Text (Text)
import qualified Data.Text as Text

import Lib

data Options = Options { bucketName :: String
                       , divideBy   :: Int
                       , region     :: Region
                       } deriving Show

regionError   = "Couldn't parse region argument. Please use standard aws region notation. example: us-west-1" 
divisionError = "Couldn't parse division argument" 

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['b'] ["bucket"] 
          ( ReqArg (\s opt -> return opt { bucketName = s }) "BUCKETNAME" 
          ) "Bucket name that contains the objects to be divided."

          , Option ['r'] ["region"] 
          ( ReqArg (\s opt -> do
              case (fromText (Text.pack s) :: Either String Region) of
                Left  err -> hPutStrLn stderr regionError >> exitFailure
                Right r   -> return opt { region = r } 
            ) "us-east-1"
          ) "Set region to operate in. S3 requires you specifiy what region the bucket is in."

          , Option ['d'] ["divideBy"] 
          ( ReqArg (\s opt -> do
              case (readEither s :: Either String Int) of
                Left  err -> hPutStrLn stderr divisionError >> exitFailure
                Right i   -> return opt { divideBy = i } 
            ) "NUM"
          ) "Divide the collection of objects N times."
           
          , Option ['h'] ["help"]
          ( NoArg (\_ -> do
              name <- getProgName
              hPutStrLn stderr (usageInfo name options)
              exitSuccess
            )
          ) "Show help."
          ]

defaultOpts = Options mempty 1 NorthVirginia

main :: IO ()
main = do
  args <- getArgs

  let (actions, nonOpts, errors) = getOpt RequireOrder options args

  opts <- foldl (>>=) (return defaultOpts) actions

  divideS3BucketBy (bucketName opts) (divideBy opts) (region opts)
