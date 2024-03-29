module BinarySearchTree(BST(Empty,Node) ,emptyBST, lookupBST, insertBST, removeBST, entriesBST) where

data BST key value = Empty | Node key value (BST key value) (BST key value) 
                    deriving (Show, Eq)

-- create empty binary search tree
emptyBST :: BST key value
emptyBST = Empty

-- | Lookup the item by specifying a key
lookupBST :: (Ord key) => key -> BST key value -> Maybe value
lookupBST _ Empty = Nothing
lookupBST newKey (Node key value left right)
    | newKey < key   = lookupBST newKey left
    | newKey > key   = lookupBST newKey right
    | otherwise = Just value

-- | Insert an entry into the BST
insertBST :: (Ord key) => key -> value -> BST key value -> BST key value
insertBST newKey newValue Empty = Node newKey newValue Empty Empty
--insertBST newKey newValue Empty = Node newKey newValue Empty Empty
insertBST newKey newValue (Node key value left right)
    | newKey < key   = Node key value (insertBST newKey newValue left) right
    | newKey > key   = Node key value left (insertBST newKey newValue right)
    | otherwise = Node newKey newValue left right

-- | Remove an entry from the BST by key
removeBST :: (Ord k) => k -> BST k v -> BST k v
removeBST _ Empty = Empty  -- if tree is empty, return empty
removeBST key (Node currKey value left right)
    | key < currKey   = Node currKey value (removeBST key left) right
    | key > currKey   = Node currKey value left (removeBST key right) 
    | otherwise = mergeSubtrees left right  

-- | Merge subtrees
mergeSubtrees :: (Ord k) => BST k v -> BST k v -> BST k v
mergeSubtrees left right = case (left, right) of
    (Empty, _) -> right  -- If left subtree is empty, return right subtree
    (_, Empty) -> left   -- If right subtree is empty, return left subtree
    _          -> Node k' v' left (removeBST k' right)  -- Merge by replacing the removed node with the leftmost node of the right subtree
        where
            (k', v') = leftmost right
            leftmost (Node k v Empty _) = (k, v)
            leftmost (Node _ _ left' _) = leftmost left'

entriesBST :: BST k v -> [(k, v)]
entriesBST tree = inorder tree []
    where
        inorder Empty acc = acc
        inorder (Node k v left right) acc = inorder left ((k, v) : inorder right acc)