-- i is the index type, while a is the value type in the list
data List i a = Append i (List i a) (List i a) | Entry a | Nil

index :: Num i => List i a -> i
index Nil            = 0
index (Entry _)      = 1
index (Append i _ _) = i

getEntry :: (Num i, Ord i) => i -> List i a -> Maybe a
getEntry n Nil              = Nothing
getEntry n (Entry a)        | n == 0 = Just a
                            | otherwise = Nothing
getEntry n (Append i l1 l2) | n < (index l1) = getEntry n l1
                            | otherwise      = getEntry (n - index l1) l2

addEntry :: (Num i, Ord i) => List i a -> i -> a -> List i a
addEntry Nil 0 a              = Entry a
addEntry (Entry a) 0 a'       = Append 2 (Entry a') (Entry a)
addEntry (Entry a) 1 a'       = Append 2 (Entry a)  (Entry a')
addEntry (Append i l1 l2) n a | n < index l1 = Append (i + 1) (addEntry l1 n a) l2
                              | otherwise    = Append (i + 1) l1 $ addEntry l2 (n - index l1) a
addEntry _                _ a = Entry a

appendList :: Num i => List i a -> List i a -> List i a
appendList l1 Nil = l1
appendList Nil l2 = l2
appendList l1 l2  = Append (index l1 + index l2) l1 l2

-- toList :: List i a -> [a]
-- toList Nil = []
-- toList (Entry a) =
-- toList (Append i l1 l2) =
