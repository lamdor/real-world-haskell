module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
  
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do 
    let (dirName, baseName) = splitFileName pat 
    dirs <- if isPattern dirName
            then namesMatching (dropTrailingPathSeparator dirName)
            else return [dirName]
    let listDir = if isPattern baseName
                  then listMatches
                  else listPlain
    pathNames <- forM dirs $ \dir -> do
      baseNames <- listDir dir baseName
      return (map (dir </>) baseNames)
      
    return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  -- handle (const (return [])) $ do -- doesn't work anymore
  names <- getDirectoryContents dirName'
  let names' = if isHidden pat
               then filter isHidden names
               else filter (not . isHidden) names
  return (filter (`matchesGlob` pat) names')

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])
