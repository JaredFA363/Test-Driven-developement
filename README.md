# Haskell TDD Project: Binary Tree and Dictionary

## Overview

This project is a Haskell implementation of a binary tree and dictionary, developed using Test-Driven Development (TDD). The project utilizes the Haskell Stack tool and GHCi for testing and development. The main objective is to demonstrate the principles of TDD by building robust and tested data structures in Haskell.

## Features

- **Binary Tree**: A basic binary tree implementation with standard operations.
- **Dictionary**: A dictionary (or map) data structure built on top of the binary tree, supporting key-value pairs.

## Setup Instructions

To set up the project, you need to have Haskell Stack installed. Follow these steps to get started:

1. **Clone the repository**:
    ```sh
    git clone https://github.com/yourusername/haskell-tdd-binary-tree.git
    cd haskell-tdd-binary-tree
    ```

2. **Install dependencies**:
    ```sh
    stack setup
    stack build
    ```

3. **Run the tests**:
    ```sh
    stack test
    ```

## Usage

You can interact with the project using GHCi for a REPL environment:

1. **Start GHCi with Stack**:
    ```sh
    stack ghci
    ```

2. **Load the BinaryTree module**:
    ```haskell
    :load src/BinaryTree.hs
    ```

3. **Use the BinaryTree functions**:
    ```haskell
    let tree = insert 5 emptyTree
    let tree = insert 3 tree
    let tree = insert 7 tree
    ```

4. **Load the Dictionary module**:
    ```haskell
    :load src/Dictionary.hs
    ```

5. **Use the Dictionary functions**:
    ```haskell
    let dict = insertDict "key1" "value1" emptyDict
    let dict = insertDict "key2" "value2" dict
    ```

## Implementation Details

### Binary Tree

The binary tree implementation includes the following features:
- Insertion of elements
- Deletion of elements
- Search for elements
- In-order traversal
- Deletion of element by Predicate

### Dictionary

The dictionary implementation uses the binary tree to store key-value pairs and includes:
- Insertion of key-value pairs
- Deletion of key-value pairs
- Lookup of values by key
- Traversal of all key-value pairs
