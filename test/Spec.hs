import Test.HUnit
import Test.QuickCheck
import BinarySearchTree(BST(Empty,Node), emptyBST, lookupBST, insertBST, removeBST, entriesBST, rotateLeft, rotateRight, removeByPredicateBST, mergeSubtrees)
import qualified Dictionary as D
import Dictionary(Diction, emptyDictionary, lookupDict, insertDict, entriesDict, removeDictEntryByKey, removeByPredicateDict)

-- Empty tree function tests
-- Testing empty binary search tree creation function
testEmptyBSTcreation :: Test
testEmptyBSTcreation = TestCase $ do
                        let tree = emptyBST :: BST Int String
                        (assertEqual "emptyBST should create an empty tree" Empty tree)

testEmptyBSTwithdifferentTypes :: Test
testEmptyBSTwithdifferentTypes = TestCase $ do
                        let tree = emptyBST :: BST Char Double
                        (assertEqual "emptyBST should create an empty tree" Empty tree)

emptyBSTTest :: Test
emptyBSTTest = TestList [testEmptyBSTcreation, testEmptyBSTwithdifferentTypes]

-- Lookup function tests
-- Testing Lookup key that exists in the tree
testLookupExistingKey :: Test
testLookupExistingKey = TestCase $ do
    let tree = Node 5 "Value" (Node 3 "Left" Empty Empty) (Node 8 "Right" Empty Empty)
    (assertEqual "Lookup existing key" (Just "Value") (lookupBST 5 tree))

testLookupNonExistingKey :: Test
testLookupNonExistingKey = TestCase $ do
    let tree = Node 5 "Value" (Node 3 "Left" Empty Empty) (Node 8 "Right" Empty Empty)
    (assertEqual "Lookup non-existing key" Nothing (lookupBST 10 tree))

testLookupEmptyTree :: Test
testLookupEmptyTree = TestCase $ do
    let tree = Empty
    assertEqual "Lookup in empty tree" (Nothing :: Maybe String) (lookupBST 5 tree)

lookupBSTTests :: Test
lookupBSTTests = TestList [ testLookupExistingKey, testLookupNonExistingKey, testLookupEmptyTree]

-- Insert function tests
-- Test cases for insert node function
testInsertEmptyTree :: Test
testInsertEmptyTree = TestCase $ do
    let tree = Empty :: BST Int String
        newTree = insertBST 5 "Value" tree
    assertEqual "Insert into empty tree" (Node 5 "Value" Empty Empty) newTree

testInsertExistingKey :: Test
testInsertExistingKey = TestCase $ do
    let tree = Node 5 "Value" Empty Empty
        newTree = insertBST 5 "NewValue" tree
    assertEqual "Insert with existing key" (Node 5 "NewValue" Empty Empty) newTree

testInsertLeftSubtree :: Test
testInsertLeftSubtree = TestCase $ do
    let tree = Node 5 "Value" (Node 3 "Left" Empty Empty) Empty
        newTree = insertBST 2 "NewLeft" tree
    assertEqual "Insert into left subtree" (Node 5 "Value" (Node 3 "Left" (Node 2 "NewLeft" Empty Empty) Empty) Empty) newTree

testInsertRightSubtree :: Test
testInsertRightSubtree = TestCase $ do
    let tree = Node 5 "Value" Empty (Node 8 "Right" Empty Empty)
        newTree = insertBST 9 "NewRight" tree
    assertEqual "Insert into right subtree" (Node 5 "Value" Empty (Node 8 "Right" Empty (Node 9 "NewRight" Empty Empty))) newTree

insertBSTTests :: Test
insertBSTTests = TestList [ testInsertEmptyTree, testInsertExistingKey, testInsertLeftSubtree, testInsertRightSubtree]

-- Merge functioon tests
-- Test cases for mergeSubtrees function
testMergeSubtreesBothEmpty :: Test
testMergeSubtreesBothEmpty = TestCase $ do
    let left = emptyBST :: BST Int String
        right = emptyBST :: BST Int String
    assertEqual "Merge subtrees with both empty" Empty (mergeSubtrees left right)

testMergeSubtreesLeftEmpty :: Test
testMergeSubtreesLeftEmpty = TestCase $ do
    let left = emptyBST :: BST Int String
        right = Node 5 "Right" Empty Empty
    assertEqual "Merge subtrees with left empty" right (mergeSubtrees left right)

testMergeSubtreesRightEmpty :: Test
testMergeSubtreesRightEmpty = TestCase $ do
    let left = Node 5 "Left" Empty Empty
        right = emptyBST :: BST Int String
    assertEqual "Merge subtrees with right empty" left (mergeSubtrees left right)

testMergeSubtreesBothNonEmpty :: Test
testMergeSubtreesBothNonEmpty = TestCase $ do
    let left = Node 3 "Left" (Node 2 "LeftChild" Empty Empty) (Node 4 "RightChild" Empty Empty)
        right = Node 6 "Right" (Node 5 "LeftChild" Empty Empty) (Node 7 "RightChild" Empty Empty)
        expected = Node 5 "LeftChild" (Node 3 "Left" (Node 2 "LeftChild" Empty Empty) (Node 4 "RightChild" Empty Empty)) (Node 6 "Right" Empty (Node 7 "RightChild" Empty Empty))
    assertEqual "Merge subtrees with both non-empty" expected (mergeSubtrees left right)

mergeSubtreesTests :: Test
mergeSubtreesTests = TestList [ testMergeSubtreesBothEmpty, testMergeSubtreesLeftEmpty, testMergeSubtreesRightEmpty, testMergeSubtreesBothNonEmpty]

-- Remove functioon tests
-- Test cases for remove node function
testRemoveExistingKeyWithNoSubtrees :: Test
testRemoveExistingKeyWithNoSubtrees = TestCase $ do
    let tree = Node 7 "X" (Node 2 "Gef" Empty Empty) Empty
        removedTree = removeBST 2 tree
    assertEqual "Removing Node with No Subtrees" (Node 7 "X" Empty Empty) removedTree

testRemoveExistingKeyWithRightSubtree :: Test
testRemoveExistingKeyWithRightSubtree = TestCase $ do
    let tree = Node 5 "u" (Node 3 "var" (Node 2 "ty" Empty Empty) (Node 4 "we" Empty Empty)) (Node 8 "ft" (Node 6 "pop" Empty Empty) (Node 32 "iok" Empty Empty))
        removedRightSubtree = removeBST 8 tree
    assertEqual "Removed node on the right with a subtree" (Node 5 "u" (Node 3 "var" (Node 2 "ty" Empty Empty) (Node 4 "we" Empty Empty)) (Node 32 "iok" (Node 6 "pop" Empty Empty) Empty)) removedRightSubtree

testRemoveExistingKeyWithLeftSubtree :: Test
testRemoveExistingKeyWithLeftSubtree = TestCase $ do
    let tree = Node 5 "u" (Node 3 "var" (Node 2 "ty" Empty Empty) (Node 4 "we" Empty Empty)) (Node 8 "ft" (Node 6 "pop" Empty Empty) (Node 32 "iok" Empty Empty))
        removedLeftSubtree = removeBST 3 tree
    assertEqual "Removed node on the right with a subtree"  (Node 5 "u" (Node 4 "we" (Node 2 "ty" Empty Empty) Empty) (Node 8 "ft" (Node 6 "pop" Empty Empty) (Node 32 "iok" Empty Empty))) removedLeftSubtree

testRemoveWithEmptyTree :: Test
testRemoveWithEmptyTree = TestCase $ do
    let tree = emptyBST :: BST Int String
        removedEmpty = removeBST 6 tree
    assertEqual "Attempting to remove from empty tree" (Empty) removedEmpty

removeBSTTests :: Test
removeBSTTests = TestList [testRemoveExistingKeyWithLeftSubtree, testRemoveExistingKeyWithNoSubtrees, testRemoveWithEmptyTree, testRemoveExistingKeyWithRightSubtree]

-- show Entries Function
-- Test cases for show entries Function
testEntriesSingleNode :: Test
testEntriesSingleNode = TestCase $ do
    let tree = Node 5 "Value" Empty Empty
        result = entriesBST tree
    assertEqual "Entries of a tree with single node" [(5, "Value")] result

testEntriesMultipleNodes :: Test
testEntriesMultipleNodes = TestCase $ do
    let tree = Node 5 "Value" (Node 3 "Left" Empty Empty) (Node 7 "Right" Empty Empty)
        result = entriesBST tree
    assertEqual "Entries of a tree with multiple nodes" [(3, "Left"), (5, "Value"), (7, "Right")] result

entriesBSTTests :: Test
entriesBSTTests = TestList [testEntriesSingleNode, testEntriesMultipleNodes]

-- Test case for rotateLeft function
testRotateLeftSingleNode :: Test
testRotateLeftSingleNode = TestCase $ do
    let tree = Node 5 "Value" Empty Empty
    assertEqual "Rotate left on single-node tree" (Node 5 "Value" Empty Empty) (rotateLeft tree)

testRotateLeftWithLeftChild :: Test
testRotateLeftWithLeftChild = TestCase $ do
    let tree = Node 5 "Root" (Node 3 "Left" Empty Empty) (Node 7 "Right" Empty Empty)
        expected = Node 7 "Right" (Node 5 "Root" (Node 3 "Left" Empty Empty) Empty) Empty
    assertEqual "Rotate left with left child" expected (rotateLeft tree)

testRotateLeftWithRightChild :: Test
testRotateLeftWithRightChild = TestCase $ do
    let tree = Node 5 "Root" (Node 3 "Left" Empty Empty) (Node 7 "Right" Empty (Node 9 "RRight" Empty Empty))
        expected = Node 7 "Right" (Node 5 "Root" (Node 3 "Left" Empty Empty) Empty) (Node 9 "RRight" Empty Empty)
    assertEqual "Rotate left with right child" expected (rotateLeft tree)

testRotateLeftDeepTree :: Test
testRotateLeftDeepTree = TestCase $ do
    let tree = Node 5 "root" (Node 3 "left" (Node 2 "l-left" Empty Empty) (Node 4 "l-right" Empty Empty)) (Node 7 "right" (Node 6 "r-left" Empty Empty) (Node 8 "r-right" Empty Empty))
        rotatedTree = rotateLeft tree
        expectedTree = Node 7 "right" (Node 5 "root" (Node 3 "left" (Node 2 "l-left" Empty Empty) (Node 4 "l-right" Empty Empty)) (Node 6 "r-left" Empty Empty)) (Node 8 "r-right" Empty Empty)
    assertEqual "Rotate left should produce expected tree" expectedTree rotatedTree

rotateLeftTests :: Test
rotateLeftTests = TestList [testRotateLeftDeepTree, testRotateLeftSingleNode, testRotateLeftWithLeftChild, testRotateLeftWithRightChild]

-- Test case for rotateRight function
testRotateRight :: Test
testRotateRight = TestCase $ do
    let tree = Node 7 "root" (Node 5 "left" (Node 3 "l-left" (Node 2 "l-left-left" Empty Empty) (Node 4 "l-left-right" Empty Empty)) (Node 6 "l-right" Empty Empty)) (Node 8 "right" Empty (Node 9 "r-right" Empty Empty))
        rotatedTree = rotateRight tree
        expectedTree = Node 5 "left" (Node 3 "l-left" (Node 2 "l-left-left" Empty Empty) (Node 4 "l-left-right" Empty Empty)) (Node 7 "root" (Node 6 "l-right" Empty Empty) (Node 8 "right" Empty (Node 9 "r-right" Empty Empty)))
    assertEqual "Rotate right should produce expected tree" expectedTree rotatedTree

-- Test cases for removeByPredicateBST function
testRemoveByPredicate :: Test
testRemoveByPredicate = TestList
    [ "Remove matching element" ~:
        removeByPredicateBST (\k _ -> k == 2) (Node 1 "one" (Node 2 "two" Empty Empty) (Node 3 "three" Empty Empty))
        ~?= Node 1 "one" Empty (Node 3 "three" Empty Empty)
    , "Remove non-matching element" ~:
        removeByPredicateBST (\k _ -> k == 4) (Node 1 "one" (Node 2 "two" Empty Empty) (Node 3 "three" Empty Empty))
        ~?= Node 1 "one" (Node 2 "two" Empty Empty) (Node 3 "three" Empty Empty)
    , "Remove from empty tree" ~:
        removeByPredicateBST (\_ _ -> True) Empty
        ~?= (Empty :: BST Int String)
    , "Remove from tree with one element" ~:
        removeByPredicateBST (\_ _ -> True) (Node 1 "one" Empty Empty)
        ~?= (Empty :: BST Int String)
    , "Remove from tree with multiple elements" ~:
        removeByPredicateBST (\k _ -> k `mod` 2 == 0) (Node 1 "one" (Node 2 "two" Empty Empty) (Node 3 "three" Empty Empty))
        ~?= Node 1 "one" Empty (Node 3 "three" Empty Empty)
    ]



-- Test case for emptyDictionary function
testEmptyDictionary :: Test
testEmptyDictionary = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
    assertEqual "Empty dictionary should have no entries" [] (entriesDict dict)

-- Test case for insertDict function
testInsertDict :: Test
testInsertDict = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        dict' = insertDict "key" 42 dict
    assertEqual "Inserting an entry should increase the number of entries" [("key", 42)] (entriesDict dict')

prop_insertEntryTest :: [(Int, String)] -> Int -> String -> Property
prop_insertEntryTest entries key value =
  let originalDictionary = foldr (\(k, v) acc -> D.insertDict k v acc) D.emptyDictionary entries
      updatedDictionary = D.insertDict key value originalDictionary
  in
  property $ D.lookupDict key updatedDictionary == Just value

-- Test case for lookupDict function
testLookupDict :: Test
testLookupDict = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        dict' = insertDict "key" 42 dict
    assertEqual "Lookup should return Just value if key exists" (Just 42) (lookupDict "key" dict')
    assertEqual "Lookup should return Nothing if key doesn't exist" Nothing (lookupDict "nonexistent" dict')

prop_LookupDict :: [(String, Int)] -> Property
prop_LookupDict kvs =
    not (null nonEmptyKvs) ==>  -- Ensure non-empty key-value pairs
    all (\(k, v) -> lookupDict k (foldr (uncurry insertDict) emptyDictionary nonEmptyKvs) == Just v) nonEmptyKvs
  where
    nonEmptyKvs = filter (not . null . fst) kvs

-- Test case for removeDictEntryByKey function
testRemoveDictEntryByKey :: Test
testRemoveDictEntryByKey = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        dict' = insertDict "key" 42 dict
        dict'' = removeDictEntryByKey "key" dict'
    assertEqual "Removing an entry should decrease the number of entries" [] (entriesDict dict'')

-- Test case for entriesDict function
testEntriesDict :: Test
testEntriesDict = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        dict' = insertDict "key1" 42 dict
        dict'' = insertDict "key2" 43 dict'
    assertEqual "Entries should match the inserted entries" [("key1", 42), ("key2", 43)] (entriesDict dict'')

dictionaryFuncTests :: Test
dictionaryFuncTests = TestList [testEmptyDictionary, testInsertDict, testLookupDict, testRemoveDictEntryByKey, testEntriesDict]

-- Test cases for removeByPredicateDict function
testRemoveValueByPredicateDict :: Test
testRemoveValueByPredicateDict  = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        dict1 =insertDict "key1" 42 dict
        predicate key value = value < 50
        predicateDict = removeByPredicateDict predicate dict1
    assertEqual "Removing by predicate using value" [] (entriesDict predicateDict)

testRemoveKeyByPredicateDict :: Test
testRemoveKeyByPredicateDict  = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        dict1 =insertDict "key1" 42 dict
        dict2 =insertDict "zkey3" 78 dict
        predicate key value = key < "l"
        predicateDict = removeByPredicateDict predicate dict2
    assertEqual "Removing by predicate using value" [("zkey3",78)] (entriesDict predicateDict)

testRemoveEmptyByPredicate :: Test
testRemoveEmptyByPredicate = TestCase $ do
    let dict = emptyDictionary :: Diction String Int
        predicate key value = key < "1"
        dict1 = removeByPredicateDict predicate dict
    assertEqual "Removing by predicate using value" [] (entriesDict dict1)

predicateDictTests :: Test
predicateDictTests = TestList [testRemoveKeyByPredicateDict, testRemoveValueByPredicateDict, testRemoveEmptyByPredicate]

main :: IO ()
main = do
  _ <- runTestTT emptyBSTTest

  _ <- runTestTT lookupBSTTests

  _ <- runTestTT insertBSTTests

  _ <- runTestTT mergeSubtreesTests

  _ <- runTestTT removeBSTTests

  _ <- runTestTT entriesBSTTests

  _ <- runTestTT rotateLeftTests

  _ <- runTestTT testRotateRight

  _ <- runTestTT testRemoveByPredicate

  _ <- runTestTT dictionaryFuncTests

  _ <- runTestTT predicateDictTests

  quickCheck prop_insertEntryTest
  quickCheck prop_LookupDict
  return ()