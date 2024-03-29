import Test.HUnit
import Test.QuickCheck
import BinarySearchTree(BST(Empty,Node), emptyBST, lookupBST, insertBST, removeBST, entriesBST)
import qualified Data.Map.Strict as Map

-- HUnit Tests

-- Empty tree function tests
-- Testing empty binary search tree creation function
testEmptyBSTcreation :: Test
testEmptyBSTcreation = TestCase $ do
                        let tree = emptyBST :: BST Int String
                        (assertEqual "emptyBST should create an empty tree" Empty tree)


-- Lookup function tests
-- Testing Lookup key that exists in the tree
testLookupExistingKey :: Test
testLookupExistingKey = TestCase $ do
    let tree = Node 5 "Value" (Node 3 "Left" Empty Empty) (Node 8 "Right" Empty Empty)
    (assertEqual "Lookup existing key" (Just "Value") (lookupBST 5 tree))

-- Testing Lookup key that does not exist in the tree
testLookupNonExistingKey :: Test
testLookupNonExistingKey = TestCase $ do
    let tree = Node 5 "Value" (Node 3 "Left" Empty Empty) (Node 8 "Right" Empty Empty)
    (assertEqual "Lookup non-existing key" Nothing (lookupBST 10 tree))

-- Test case: Lookup key in an empty tree
testLookupEmptyTree :: Test
testLookupEmptyTree = TestCase $ do
    let tree = Empty
    assertEqual "Lookup in empty tree" (Nothing :: Maybe String) (lookupBST 5 tree)

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

-- Remove & Merge functioon tests
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
    assertEqual "Removed node on the right with a subtree" (Node 5 "u" (Node 3 "var" (Node 2 "ty" Empty Empty) (Node 4 "we" Empty Empty)) (Node 8 "ft" (Node 6 "pop" Empty Empty) (Node 32 "iok" Empty Empty))) removedRightSubtree

testRemoveExistingKeyWithLeftSubtree :: Test
testRemoveExistingKeyWithLeftSubtree = TestCase $ do
    let tree = Node 5 "u" (Node 3 "var" (Node 2 "ty" Empty Empty) (Node 4 "we" Empty Empty)) (Node 8 "ft" (Node 6 "pop" Empty Empty) (Node 32 "iok" Empty Empty))
        removedLeftSubtree = removeBST 3 tree
    assertEqual "Removed node on the right with a subtree" (Node 5 "u" (Node 3 "var" (Node 2 "ty" Empty Empty) (Node 4 "we" Empty Empty)) (Node 8 "ft" (Node 6 "pop" Empty Empty) (Node 32 "iok" Empty Empty))) removedLeftSubtree

--testRemoveWithNonExistingKey :: Test
--testRemoveWithNonExistingKey = TestCase $ do

testRemoveWithEmptyTree :: Test
testRemoveWithEmptyTree = TestCase $ do
    let tree = emptyBST :: BST Int String
        removedEmpty = removeBST 6 tree
    assertEqual "Attempting to remove from empty tree" (Empty) removedEmpty

-- show Entries Function
-- Test cases for show entries Function
testEntriesEmptyTree :: Test
testEntriesEmptyTree = TestCase $ do
    let tree = Empty
        result = entriesBST tree
    assertEqual "Entries of an empty tree" ([] :: [(Int, String)]) result

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

-- Property Based Tests

-- Property: After inserting a key-value pair into the BST, the key should be present in the tree
--prop_insertedKeyExists :: Ord k => k -> v -> Map.Map k v -> BST k v -> Property
--prop_insertedKeyExists key value mapTree bstTree =
--    let newMap = Map.insert key value mapTree
--        newBST = insertBST key value bstTree
--    in property $ Map.lookup key newMap == lookup key newBST

-- Inserted key should be present in the tree
-- prop_InsertedKeyPresent :: Int -> Int -> BST Int Int -> Property
-- prop_InsertedKeyPresent key value tree =
  -- let updatedTree = insertBST key value tree
  -- in lookupBST key updatedTree === Just value

main :: IO ()
main = do
  _ <- runTestTT testEmptyBSTcreation

  _ <- runTestTT testLookupExistingKey
  _ <- runTestTT testLookupNonExistingKey
  _ <- runTestTT testLookupEmptyTree

  _ <- runTestTT testInsertEmptyTree
  _ <- runTestTT testInsertExistingKey
  _ <- runTestTT testInsertLeftSubtree
  _ <- runTestTT testInsertRightSubtree
  _ <- runTestTT testRemoveExistingKeyWithNoSubtrees

  _ <- runTestTT testRemoveExistingKeyWithNoSubtrees
  _ <- runTestTT testRemoveExistingKeyWithRightSubtree
  _ <- runTestTT testRemoveExistingKeyWithLeftSubtree
  _ <- runTestTT testRemoveWithEmptyTree

  _ <- runTestTT testEntriesEmptyTree
  _ <- runTestTT testEntriesSingleNode
  _ <- runTestTT testEntriesMultipleNodes


  --quickCheck prop_InsertedKeyPresent
  --quickCheck prop_insertedKeyExists
  return ()