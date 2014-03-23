-- exercise 1,2
module GlobRegexEither
       (
         globToRegex
       , matchesGlob
       , GlobError
       ) where

import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = rappend '^' $ fmap (\re -> re ++ "$") $ globToRegex' cs

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = rconcat ".*" $ globToRegex' cs
globToRegex' ('?':cs) = rconcat "." $ globToRegex' cs

globToRegex' ('[':'!':c:cs) = rconcat "[^" $ rappend c $ charClass cs
globToRegex' ('[':c:cs) = rconcat "[" $ rappend c $ charClass cs
globToRegex' ('[':_) = Left "unterminated character class"

globToRegex' (c:cs) = rconcat (escape c) $ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}|"

charClass :: String -> Either GlobError String
charClass (']':cs) = rappend ']' $ globToRegex' cs
charClass (c:cs) = rappend c $ charClass cs
charClass [] = Left "unterminated character class"

rappend :: b -> Either a [b] -> Either a [b]
rappend b = fmap ((:) b)

rconcat :: [b] -> Either a [b] -> Either a [b]
rconcat bs = fmap ((++) bs)

matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` pat = fmap ((=~) name) $ globToRegex pat
