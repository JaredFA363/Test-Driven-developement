module BinarySearchTree(BST(Empty,Node) ,emptyBST, lookupBST, insertBST, removeBST, entriesBST, removeByPredicateBST,rotateLeft,rotateRight, removeByPredicateBST, mergeSubtrees) where

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
insertBST newKey newValue (Node key value left right)
    | newKey < key   = Node key value (insertBST newKey newValue left) right
    | newKey > key   = Node key value left (insertBST newKey newValue right)
    | otherwise = Node newKey newValue left right

-- | Remove an entry from the BST by key
removeBST :: (Ord key) => key -> BST key value -> BST key value
removeBST _ Empty = Empty  -- if tree is empty, return empty
removeBST key (Node currKey value left right)
    | key < currKey   = Node currKey value (removeBST key left) right
    | key > currKey   = Node currKey value left (removeBST key right) 
    | otherwise = mergeSubtrees left right  

-- | Merge subtrees
mergeSubtrees :: (Ord key) => BST key value -> BST key value -> BST key value
mergeSubtrees left right = case (left, right) of
    (Empty, _) -> right  -- If left subtree empty return right subtree
    (_, Empty) -> left   -- If right subtree empty return left subtree
    _          -> Node newSubtreeKey newSubtreeVal left (removeBST newSubtreeKey right)  -- Merge by replacing the removed node with the leftmost node of the right subtree
        where
            (newSubtreeKey, newSubtreeVal) = leftmost right
            leftmost (Node key value Empty _) = (key, value)
            leftmost (Node _ _ left' _) = leftmost left'

entriesBST :: BST key value-> [(key, value)]
entriesBST tree = inorder tree []
    where
        inorder Empty acc = acc
        inorder (Node key value left right) acc = inorder left ((key, value) : inorder right acc)

-- Function to rotate a BST to the left
rotateLeft :: BST key value -> BST key value
rotateLeft Empty = Empty
rotateLeft (Node parentKey parentValue leftChild rightChild@(Node rightChildKey rightChildValue rightLeftChild rightRightChild)) =
    Node rightChildKey rightChildValue (Node parentKey parentValue leftChild rightLeftChild) rightRightChild
rotateLeft tree = tree  -- No rotation needed if the right child is Empty or a leaf node

-- Function to rotate a BST to the right
rotateRight :: BST key value -> BST key value
rotateRight Empty = Empty
rotateRight (Node parentKey parentValue leftChild@(Node leftChildKey leftChildValue leftLeftChild leftRightChild) rightChild) =
    Node leftChildKey leftChildValue leftLeftChild (Node parentKey parentValue leftRightChild rightChild)
rotateRight tree = tree  -- No rotation needed if the left child is Empty or a leaf node

removeByPredicateBST :: (key -> value -> Bool) -> BST key value -> BST key value
removeByPredicateBST _ Empty = Empty
removeByPredicateBST predicate (Node key value left right)
    | predicate key value = removeNode (Node key value left right)
    | otherwise = Node key value (removeByPredicateBST predicate left) (removeByPredicateBST predicate right)
    where
        removeNode :: BST key value -> BST key value
        removeNode (Node _ _ Empty right) = right
        removeNode (Node _ _ left Empty) = left
        removeNode (Node _ _ left right) = Node successorKey successorValue newLeft right
            where
                (successorKey, successorValue, newLeft) = deleteMax left

        deleteMax :: BST key value -> (key, value, BST key value)
        deleteMax (Node key value left Empty) = (key, value, left)
        deleteMax (Node key value left right) = (maxKey, maxValue, Node key value left newRight)
            where
                (maxKey, maxValue, newRight) = deleteMax right