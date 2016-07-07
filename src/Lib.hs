{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( divideS3BucketBy
    ) where

import Data.Conduit (sourceToList)
import Data.Maybe
import Data.Monoid

import Control.Lens
import Control.Lens.Prism
import Control.Monad
import Control.Monad.IO.Class

import Network.AWS
import Network.AWS.S3
import Network.AWS.S3.Types

import System.IO

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

type NamedObjects a = (String, [a])

assignArrayNames :: Int -> [[Object]] -> [NamedObjects Object]
assignArrayNames _ [] = []
assignArrayNames i (x:xs) = (name, x) : assignArrayNames (i+1) xs
  where
    name = "s3-" <> show i

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = f : (splitEvery n r)
  where 
    (f, r) = splitAt n list

getObjectsInBucket :: MonadAWS m => Text -> m [Object]
getObjectsInBucket name = do
  rs <- sourceToList . paginate . listObjects . BucketName $ name
  let xs :: [Object]
      xs = rs ^. traverse . lorsContents
  return xs

divideS3BucketBy :: String -> Int -> Region -> IO ()
divideS3BucketBy bucket divideBy region = do
  env <- newEnv region Discover

  let bucket' = Text.pack bucket

  void . runResourceT . runAWS env $ do
    objs <- getObjectsInBucket bucket'
    let objects :: [Object]
        objects = objs

        len = length objs
      
        (d, m) = len `divMod` divideBy
 
        sObj :: [[Object]]
        sObj = splitEvery (d + m) objs

        namedObjs = assignArrayNames 0 sObj

    liftIO . mapM openAndWriteObjects $ namedObjs

  where
    printKeyToHandle :: Handle -> Object -> IO ()
    printKeyToHandle h o = Text.hPutStrLn h $ o ^. oKey . _ObjectKey

    openAndWriteObjects :: NamedObjects Object -> IO ()
    openAndWriteObjects (name, objs) = do
      h <- openFile name WriteMode
      mapM (printKeyToHandle h) objs
      hClose h

