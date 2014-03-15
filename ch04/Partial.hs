import Data.List (isInfixOf)

isInAny needle haystack = any (needle `isInfixOf`) haystack
