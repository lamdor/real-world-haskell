module ControlledVisit  where

import Control.Exception (IOException, bracket, handle)
import Control.Monad (forM, liftM)
import Data.Time.Clock (UTCTime(..))
import System.Directory
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hClose, hFileSize, openFile)

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)


traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info ->
    if isDirectory info && infoPath info /= path
       then traverse order (infoPath info)
       else return [info]

getInfo :: FilePath -> IO Info
getInfo path = do
  perms    <- maybeIO $ getPermissions path
  size     <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
  modified <- maybeIO $ getModificationTime path
  return (Info path perms size modified)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ignoreExceptionToNothing (Just `liftM` act)

ignoreExceptionToNothing :: IOException -> IO (Maybe a)
ignoreExceptionToNothing _ = return Nothing
