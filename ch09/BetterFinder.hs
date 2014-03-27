import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, doesDirectoryExist)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (IOException, bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -- path to directory entry
              -> Permissions -- permissions
              -> Maybe Integer -- file size (nothing if not file)
              -> UTCTime -- last modified
              -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms    <- getPermissions name
          size     <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = do
  handle ignoreIOExceptionForFileSize $
    bracket (openFile path ReadMode) hClose $ \h -> do
      size <- hFileSize h
      return (Just size)
      
ignoreIOExceptionForFileSize :: IOException -> IO (Maybe Integer)
ignoreIOExceptionForFileSize _ = return Nothing

type InfoP a = FilePath -- path to directory entry
            -> Permissions -- permissions
            -> Maybe Integer -- file size (nothing if not file)
            -> UTCTime -- last modified
            -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path
                   
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

constP :: a -> InfoP a
constP k w x y z = k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w
         
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)
                   
myTest path _ (Just size) _ = takeExtension path == ".hs" && size > 10
myTest _ _ _ _ = False

myTest2 = (liftPath takeExtension `equalP` ".hs") `andP`
          (sizeP `greaterP` 10)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP
infix 4 ==?
        
(&&?) = andP
infixr 3 &&?

(||?) = orP
infixr 2 ||?

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
infix 4 >?

myTest3 = liftPath takeExtension ==? ".hs" &&? sizeP >? 10
