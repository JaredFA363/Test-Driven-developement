module Dictionary (Diction,emptyDictionary,insertDict,lookupDict,removeDictEntryByKey,entriesDict, removeByPredicateDict) where

import BinarySearchTree (BST(Empty), emptyBST, insertBST, lookupBST, entriesBST, removeBST, removeByPredicateBST)

-- Define Dictionary data structure
newtype Diction key value = Diction (BST key value)
    deriving (Show, Eq)

-- | Create an empty dictionary
emptyDictionary :: Diction key value
emptyDictionary = Diction emptyBST

-- | Insert an entry into the dictionary
insertDict :: (Ord key) => key -> value -> Diction key value -> Diction key value
insertDict key value (Diction bst) = Diction (insertBST key value bst)

-- | Lookup the value associated with a key in the dictionary
lookupDict :: (Ord key) => key -> Diction key value -> Maybe value
lookupDict key (Diction bst) = lookupBST key bst

-- | Remove an entry from the dictionary by key
removeDictEntryByKey :: (Ord key) => key -> Diction key value -> Diction key value
removeDictEntryByKey key (Diction bst) = Diction (removeBST key bst)

-- | Show the entries of the dictionary
--showDictEntries :: (Show key, Show value) => Diction key value-> String
--showDictEntries (Diction bst) = unlines $ map (\(key, value) -> show key ++ ": " ++ show value) (entriesBST bst)

entriesDict :: Diction key value -> [(key, value)]
entriesDict (Diction bst) = entriesBST bst

-- Remove key-value pairs from a dictionary (BST) based on a predicate
removeByPredicateDict :: (key -> value -> Bool) -> Diction key value -> Diction key value
removeByPredicateDict _ (Diction Empty) = Diction Empty
removeByPredicateDict predicate (Diction bst) = Diction (removeByPredicateBST predicate bst)