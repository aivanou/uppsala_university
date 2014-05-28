module Game where

import qualified Data.List as L
import qualified Data.Map.Lazy as M

--
-- Data types and related helper functions
--

-- X or O marks. We do need one to specify an empty place because we will use
-- Maybe
data Mark = X | O deriving (Show, Eq)

-- Just one row of the board. Columns are unnecessary because we can transpose
-- the board at no cost
data Row = Row { index  :: Int
               , marks  :: [Maybe Mark]
               }

type Grid = M.Map (Int, Int) Mark
-- The board. The transposed variable does not actually change the grid, only
-- the functions that use it change their behaviour
data Board = Board { rows :: Int
                   , columns :: Int
                   , grid :: Grid
                   , isTransposed :: Bool
                   } deriving (Eq)

-- Pretty printing of the Board
instance Show Board where
    show b = "Size: " ++ show (rows b) ++ "x" ++ show (columns b) ++ " "
                      ++ (if isTransposed b then "(T)" else "")
                      ++ "\n"
                      ++ ' ' : L.intersperse ' ' (showGrid b)

-- Pretty printing of Rows
instance Show Row where
    show r = "Row[" ++ show (index r) ++ "] = "
                    ++ concatMap (\mm -> case mm of
                                        Just m -> show m ++ " "
                                        Nothing -> ". ")
                           (marks r)

-- When comparing rows, we only care about the marks and not where they are on
-- the grid
instance Eq Row where
    a == b = marks a == marks b

-- String representation of the grid
showGrid :: Board -> String
showGrid b = reverse $ tail $ reverse $ unlines -- Remove the extra '\n'
             $ splitEvery (columns b)
             $ map (\(i, j) -> case get b (i, j) of
                                Just X  -> 'X'
                                Just O  -> 'O'
                                Nothing -> '.')
                   [(i, j) | i <- [1..rows b], j <- [1..columns b]]
    where splitEvery _ [] = []
          splitEvery n xs = first : (splitEvery n rest)
                where (first, rest) = splitAt n xs

-- Initial empty board of size nxm
emptyBoard :: Int -> Int -> Board
emptyBoard n m = Board { rows = n
                       , columns = m
                       , grid = M.empty
                       , isTransposed = False
                       }

-- Check if the given index is within the boundaries of the board
inBounds :: (Int, Int) -> Board -> Bool
inBounds (i, j) b = i >= 1 && j >= 1 && i <= rows b && j <= columns b

-- Get the mark at the given index
get :: Board -> (Int, Int) -> Maybe Mark
get b (i,j) = M.lookup ix (grid b)
    where ix = if isTransposed b then (j,i) else (i,j)

-- Put the given mark at the given index
put :: Board -> (Int, Int) -> Mark -> Board
put b (i,j) m = if (i,j) `inBounds` b
                    then b { grid = M.insert ix m (grid b) }
                    else error $ "Invalid index " ++ show (i,j) ++  " for insert"
    where ix = if isTransposed b then (j,i) else (i,j)

-- Erase a mark from the given index
erase :: Board -> (Int, Int) -> Board
erase b (i,j) = if (i,j) `inBounds` b
                    then b { grid = M.delete ix (grid b) }
                    else error $ "Invalid index " ++ show (i,j) ++  " for erase"
    where ix = if isTransposed b then (j,i) else (i,j)


-- We shouldn't need to erase marks but this functions supports it. The main
-- purpose of this function is to be used by putRow
putMaybe :: Board -> (Int, Int) -> Maybe Mark -> Board
putMaybe b ix mm = b { grid = grid' }
    where grid' = grid $ case mm of
                            Just m -> put b ix m
                            Nothing -> erase b ix

-- Transpose the board. In the spirit of laziness, don't actually change the
-- grid. The functions that work with the grid should take responsibility
transpose :: Board -> Board
transpose b = b { isTransposed = not (isTransposed b)
                , rows         = columns b
                , columns      = rows b
                }

-- Transpose the indices. Partically just exchange i with j
transposeIx :: (Int, Int) -> (Int, Int)
transposeIx (i,j) = (j,i)

-- Get a row from the board
getRow :: Board -> Int -> Row
getRow b i = Row { index = i
                 , marks = map (\j -> get b (i, j)) [1..columns b]
                 }

-- Put the elements of the row back into the board
putRow :: Board -> Row ->  Board
putRow b r = if i >= 1 && i <= rows b
                then b { grid = grid' }
                else error "Invalid row index"
    where i = index r
          grid' = grid $ L.foldl'
                            (\b' (v, j) -> putMaybe b' (i, j) v)
                            b
                            (zip (marks r) [1..columns b]) -- (mark, column)

--
-- Solving the puzzle
--

-- Count the number of occurences of the given mark in the given row
countMark :: Row -> Maybe Mark -> Int
countMark r mm = length $ mm `L.elemIndices` marks r

-- Same as (countMark X, countMark O)
countMarks :: Row -> (Int, Int)
countMarks r = (countMark r (Just X), countMark r (Just O))

-- Check if there are no empty spaces in the row
isRowComplete :: Row -> Bool
isRowComplete r = not $ Nothing `elem` (marks r)

-- Check if the row is valid, ONLY in relation to itself and not to other rows
-- in the board
isRowValid :: Row -> Bool
isRowValid r = (not $ any (>2) (groupMarks mms)) && xs <= half && os <= half
    where mms        = marks r
          groupMarks = (map length) -- Get the length of each group of marks
                     . (filter ((Nothing /=) . head)) -- Ignore the empty spaces
                     . L.group -- Group consecutive marks together
          (xs, os)   = countMarks r
          half       = length mms `div` 2

-- Check if the full (ie complete) row is indeed complete and also valid
isFullRowValid :: Row -> Bool
isFullRowValid r = xs == (lms `div` 2)
                && os == (lms `div` 2)
                && isRowValid r
    where (xs, os) = countMarks r
          mms      = marks r
          lms      = length mms

-- Complete triplets of marks in the row based on the rule that no more that two
-- of the same marks should be together in a row. To do it keep both the next
-- elements of the marks list and the previous one (updated accordingly) in two
-- lists and check the ones surrounding our current mark. Essentially it moves a
-- five-mark window and checks the triplets within it
completeTriplets :: Row -> Row
completeTriplets r = completeTriplets' [] (marks r)
    where completeTriplets' ps []      = r { marks = reverse ps }
          completeTriplets' ps (mm:ms)
              | Just _  <- mm = completeTriplets' (mm:ps) ms -- Skip the case
              | Nothing <- mm = completeTriplets' (m:ps) ms
                  where
                      m = case (last2, last1, mm, next1, next2) of
                          (Just X, Just X, Nothing, _     , _     ) -> Just O
                          (_     , Just X, Nothing, Just X, _     ) -> Just O
                          (_     , _     , Nothing, Just X, Just X) -> Just O
                          (Just O, Just O, Nothing, _     , _     ) -> Just X
                          (_     , Just O, Nothing, Just O, _     ) -> Just X
                          (_     , _     , Nothing, Just O, Just O) -> Just X
                          _                                         -> mm
                      -- The following functions return Nothing if the list is
                      -- not long enough (normally head and tail throw
                      -- exceptions)
                      last1 = if null ps then Nothing else head ps
                      next1 = if null ms then Nothing else head ms
                      last2 = if null ps || null tps then Nothing else head tps
                      next2 = if null ms || null tms then Nothing else head tms
                      tps = tail ps
                      tms = tail ms


-- Take a function that works on rows/columns and apply it to all the rows and
-- columns of the board. Keep doing so until no more changes are made to the
-- board
applyToAll :: (Row -> Row) -> Board -> Board
applyToAll f b = if b' == b then b' else applyToAll f b'
    where b'    = (transpose . aTA . transpose . aTA) b
          aTA b = L.foldl'
                    (\b' i -> putRow b' (f $ getRow b' i))
                    b [1..rows b]

-- Complete all the triples in the board
completeAllTriplets :: Board -> Board
completeAllTriplets = applyToAll completeTriplets

-- Fill the emtpy spaces in the row using marks from the supplied list
fillRow :: Row -> [Mark] -> Row
fillRow r ms = r { marks = replaceEmpty (marks r) (map return ms) }
    where replaceEmpty = replaceBy (\m -> m == Nothing)

-- Replace all the elements of list a that match the predicate f with elements
-- from the list b
replaceBy :: (a -> Bool) -> [a] -> [a] -> [a]
replaceBy _ []     _      = []
replaceBy _ as     []     = as
replaceBy f (a:as) rs@(b:bs) = if f a
                                then b : replaceBy f as bs
                                else a : replaceBy f as rs

-- Complete a row where one type of mark is already exhausted
completeFullRow :: Row -> Row
completeFullRow r = fill xs os
    where (xs, os) = countMarks r
          ms       = marks r
          full     = length ms `div` 2
          fill xs os -- Use the magic of guards to avoid multiple ifs
            | xs == full = fillRow r (repeat O)
            | os == full = fillRow r (repeat X)
            | otherwise  = r

-- Complete all the full rows and columns of the board
completeAllFull :: Board -> Board
completeAllFull = applyToAll completeFullRow

-- Try all the simple completion methods for as long as changes are made
completeSimple :: Board -> Board
completeSimple b = if b' == b then b' else completeSimple b'
    where b' = (completeAllFull . completeAllTriplets) b

-- Check if the element is unique (or not at all) in the list given
unique :: (Eq a) => a -> [a] -> Bool
unique e []     = True
unique e (x:xs) = (e /= x || e `notElem` xs) && unique e xs

-- Check if all the completed rows of the board are unique
hasUniqueRows :: Board -> Bool
hasUniqueRows b = and $ map (flip unique allRows) allRows
    where allRows = filter isRowComplete (map (getRow b) [1..rows b])

-- Check if all the individual rows and columns are valid and also that there
-- are not duplicates
isBoardValid :: Board -> Bool
isBoardValid b = iBV b && iBV (transpose b)
    where iBV b     = (and $ map isRowValid (allRows b)) && hasUniqueRows b
          allRows b = map (getRow b) [1..rows b]

-- Complete using all the simple methods of completion and verify that the
-- result is valid. Also verify before completing to save time if the board is
-- already invalid
completeAndVerify :: Board -> Bool
completeAndVerify b = isBoardValid b && (isBoardValid . completeSimple) b

-- Check if either X or O are invalid choices for a given place in the grid
speculateOne :: Board -> (Int, Int) -> Maybe Mark
speculateOne b ix@(i, j) = case (tryx, tryo) of
                                (True, False) -> Just X
                                (False, True) -> Just O
                                _             -> Nothing
    where tryx = completeAndVerify $ put b ix X
          tryo = completeAndVerify $ put b ix O

-- Speculate for each empty grid square and fill in those that can. Unlike the
-- other completion methods, this only runs once
speculateAll :: Board -> Board
speculateAll b = L.foldl'
                    (\b' ix -> case speculateOne b' ix of
                                    Just m -> put b' ix m
                                    _      -> b')
                    b allGrid
    where allGrid = [(i, j) | i <- [1..rows b], j <- [1..columns b]]

-- Perform all determinist completion methods on the board
completeDet :: Board -> Board
completeDet b = if b' == b then b' else completeDet b'
    where b' = (speculateAll . completeSimple) b

-- Perform all the completion methods available
solve :: Board -> Board
solve b = if b' == b then b' else solve b'
    where b' = completeDet b

