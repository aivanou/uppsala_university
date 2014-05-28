{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Monad
import qualified Data.Map.Lazy as M

import Game

import Test.QuickCheck
import Test.QuickCheck.All
import Test.HUnit


--
-- Testing
--

instance Arbitrary Mark where
    arbitrary = elements [X, O]

instance Arbitrary Grid where
    arbitrary = do i <- choose (1, 10) :: Gen Int
                   j <- choose (1, 10) :: Gen Int
                   m <- arbitrary
                   oneof [ return (M.insert (i,j) m M.empty)
                         , liftM2 M.union arbitrary arbitrary
                         ]

instance Arbitrary Board where
    arbitrary = do r <- choose (2, 5)
                   c <- choose (2, 5)
                   m <- arbitrary
                   t <- arbitrary
                   return $ Board (2*r) (2*c) m t

-- Check if inBounds works correctly for transposed grids
prop_in_bounds_transposed :: (Int, Int) -> Bool
prop_in_bounds_transposed (i,j) =
    (i,j) `inBounds` testBoard68 == (j, i) `inBounds` transpose testBoard68

-- Check if get on an emtpy board returns always Nothing
prop_get_empty :: (Int, Int) -> Bool
prop_get_empty ix = get (emptyBoard 10 10) ix == Nothing

-- Check if get handles transposition correctly
prop_get_transposed :: (Int, Int) -> Bool
prop_get_transposed (i, j) = get testBoard68 (i, j) == get (transpose testBoard68) (j, i)

-- Check if get out of bounds returns nothing
prop_get_bounds :: (Int, Int) -> Gen Prop
prop_get_bounds ix = not (inBounds ix eB) ==> get eB ix == Nothing
    where eB = emptyBoard 0 0

-- Check if get after put brings back the correct result
prop_put_get :: (Int, Int) -> Gen Prop
prop_put_get ix = ix `inBounds` eB ==> get (put eB ix X) ix == Just X
                                    && get (put eB ix O) ix == Just O
    where eB = testBoardXL

-- Check if get after erase returns Nothing
prop_erase_get :: (Int, Int) -> Gen Prop
prop_erase_get ix = ix `inBounds` tb ==> get (erase (put tb ix O) ix) ix == Nothing
    where tb = testBoardXL

-- Check if putMaybe works as a combination of put and erase
prop_put_maybe :: (Int, Int) -> Gen Prop
prop_put_maybe ix = ix `inBounds` tb ==> get (putMaybe tb ix (Just X)) ix == Just X
                                      && get (putMaybe tb ix (Just O)) ix == Just O
                                      && get (putMaybe tb ix (Nothing)) ix == Nothing
    where tb = testBoardXL

-- Transpose should not change the grid
prop_transpose_grid :: Board -> Bool
prop_transpose_grid b = grid b == grid (transpose b)

-- Transposing twice should return the board to it's original condition
prop_transpose_twice :: Board -> Bool
prop_transpose_twice b = b == (transpose . transpose) b

-- getRow should return a row a big as the number of columns
prop_getrow_length :: Board -> Bool
prop_getrow_length b = length (marks $ getRow b 1) == columns b

-- getRow after putRow after getRow should be the same
prop_getputget :: Board -> Bool
prop_getputget b = r1 == r2
    where r1 = getRow b 1
          b' = putRow b r1
          r2 = getRow b' 1


-- Using random boards for checking the logic of the game is impractical since
-- the generation requirements are too many. We will use unit tests instead.

allTests = [ "test_counts" ~: "Just Os" ~: (0, 2) ~=? (countMarks testRow66C2)
           , "test_counts" ~: "Just Xs" ~: (2, 0) ~=? (countMarks testRow66C4)
           , "test_counts" ~: "Both"    ~: (1, 1) ~=? (countMarks testRow66R4)
           , "test_full"   ~: "Full"    ~: True   ~=? (isRowComplete testRow1Full)
           , "test_full"   ~: "Not F"   ~: False  ~=? (isRowComplete testRow1NotF)
           , "test_valid"  ~: "Valid"   ~: True   ~=? (isRowValid testRow1NotF)
           , "test_valid"  ~: "Valid"   ~: True   ~=? (isRowValid testRow1Full)
           , "test_valid"  ~: "Invalid" ~: False  ~=? (isRowValid testRow1Inv1)
           , "test_valid"  ~: "Invalid" ~: False  ~=? (isRowValid testRow1Inv2)
           , "test_valid"  ~: "Full V"  ~: True   ~=? (isFullRowValid testRow1Full)
           , "test_valid"  ~: "Full I"  ~: False  ~=? (isFullRowValid testRow1Inv2)
           , "test_pairs"  ~: "Os"      ~: testRow66C2C ~=? (completeTriplets testRow66C2)
           , "test_pairs"  ~: "Xs"      ~: testRow66C4C ~=? (completeTriplets testRow66C4)
           , "test_pairs"  ~: "Mixed"   ~: testRow1Full ~=? (completeTriplets testRow1NotF)
           , "test_fill"   ~: ""        ~: testRow1Full ~=? (fillRow testRow1NotF [O, X, X])
           , "test_com_f"  ~: ""        ~: (completeFullRow testRow21) ~=? (completeFullRow testRow22)
           , "test_all"    ~: "Board 6" ~: testBoard66F ~=? (solve testBoard66)
           ]


testBoardXL = emptyBoard maxInt maxInt
    where maxInt = maxBound :: Int

testBoard66 = Board 6 6 (M.fromList [((2,1),X),((2,6),X),((3,4),X),((4,1),O),
                                        ((4,4),X),((5,2),O),((6,2),O),((6,5),X)])
                    False
testBoard66F = Board 6 6 (M.fromList [((1,1),O),((1,2),X),((1,3),X),((1,4),O),((1,5),O),((1,6),X)
                                     ,((2,1),X),((2,2),O),((2,3),X),((2,4),O),((2,5),O),((2,6),X)
                                     ,((3,1),O),((3,2),X),((3,3),O),((3,4),X),((3,5),X),((3,6),O)
                                     ,((4,1),O),((4,2),X),((4,3),O),((4,4),X),((4,5),O),((4,6),X)
                                     ,((5,1),X),((5,2),O),((5,3),X),((5,4),O),((5,5),X),((5,6),O)
                                     ,((6,1),X),((6,2),O),((6,3),O),((6,4),X),((6,5),X),((6,6),O)
                                     ]) False

testBoard88 = Board 8 8 (M.fromList [((1,5),X),((1,8),O),((2,1),O),((3,2),X),
                                        ((3,3),O),((3,7),O),((4,4),X),((4,5),X),
                                        ((5,1),X),((5,6),O),((6,4),X),((6,8),O),
                                        ((7,7),X),((8,1),O),((8,3),O),((8,6),O)])
                    False

testBoard68 = Board 6 8 (M.fromList [((1,5),X),((1,8),O),((2,1),O),((3,2),X),
                                        ((3,3),O),((3,7),O),((4,4),X),((4,5),X),
                                        ((5,1),X),((5,6),O),((6,4),X),((6,8),O),
                                        ((7,7),X),((8,1),O),((8,3),O),((8,6),O)])
                    False

testBoard86 = Board 8 6 (M.fromList [((1,5),X),((1,8),O),((2,1),O),((3,2),X),
                                        ((3,3),O),((3,7),O),((4,4),X),((4,5),X),
                                        ((5,1),X),((5,6),O),((6,4),X),((6,8),O),
                                        ((7,7),X),((8,1),O),((8,3),O),((8,6),O)])
                    False

testRow66R4 = getRow testBoard66 4
testRow66C2 = getRow (transpose testBoard66) 2
testRow66C2C = testRow66C2 { marks = [Nothing, Nothing, Nothing, Just X, Just O, Just O] }
testRow66C4 = getRow (transpose testBoard66) 4
testRow66C4C = testRow66C4 { marks = [ Nothing, Just O, Just X, Just X, Just O, Nothing] }
testRow1Full = Row { index = 1, marks = [Just X, Just O,  Just X, Just X,  Just O, Just O, Just X, Just O] }
testRow1NotF = Row { index = 1, marks = [Just X, Nothing, Just X, Nothing, Just O, Just O, Nothing, Just O] }
testRow1Inv1  = Row { index = 1, marks = [Just X, Just X, Just X, Just X, Just O, Just O, Just X, Nothing] }
testRow1Inv2  = Row { index = 1, marks = [Just X, Just O,  Just X, Just O, Just O, Just X, Just O, Just O] }
testRow21 = Row { index = 2, marks = [ Just X, Nothing, Just X, Nothing] }
testRow22 = Row { index = 2, marks = [ Nothing, Just O, Nothing, Just O] }


failBoard = Board 6 2 (M.fromList [((4,1),X), ((5,1),X), ((6,1),O)]) False

main = do putStrLn "Starting with QuickCheck tests"
          $(quickCheckAll)
          putStrLn "Continuing with HUnit tests"
          counts <- runTestTT $ test allTests
          putStrLn (show $ counts)
          putStrLn "All tests completed"
