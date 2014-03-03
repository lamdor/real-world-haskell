data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList (Cons a l) = a : (fromList l)
fromList Nil = []
