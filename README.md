# COMP30020-assignment1
Assignment 1 for COMP30020 - Declarative Programming. A 'Fill-in puzzle' solver written in Prolog

# Overview  :
The program implements a fill-in puzzle solver, which takes a Puzzle and a WordList, and solves the Puzzle by creating slots which consist of fill-able and pre-filled sequences from the puzzle and matches words from the WordList with these slots, by finding the best slot that can be filled with the best word to be filled with.

# Usage
Load up SWI Prolog and load in the test_cases.pl file with: `[test_cases].`.
Run tests by typing `test(N)` where is N is the test numbe, and pressing enter/return.

# Game Description
A fill-in puzzle (sometimes called a fill-it-in) is like a crossword puzzle, except that instead of being given obscure clues telling us which words go where, you are given a list of all the words to place in the puzzle, but not told where they go.

The puzzle consists of a grid of squares, most of which are empty, into which letters or digits are to be written, but some of which are filled in solid, and are not to be written in. You are also given a list of words to place in the puzzle.
You must place each word in the word list exactly once in the puzzle, either left-to-right or top-to-bottom, filling a maximal sequence of empty squares. Also, every maximal sequence of non-solid squares that is more than one square long must have one word from the word list written in it. Many words cross one another, so many of the letters in a horizontal word will also be a letter in a vertical word. For a properly constructed fill-in puzzle, there will be only one way to fill in the words (in some cases, the puzzle is symmetrical around a diagonal axis, in which case there will be two symmetrical solutions).

# Results
Final Mark: 95.00%
