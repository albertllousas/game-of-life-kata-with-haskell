module GameOfLife where

import Data.List

data Cell = Dead | Alive deriving (Eq, Show)

data Grid =  Grid { cells:: [[Cell]], rows:: Int, cols:: Int } deriving (Eq, Show)

acc rows columns = Grid { cells = [], rows = rows, cols = columns }

nextGeneration :: Grid -> Grid
nextGeneration grid = if validGrid grid then nextGrid (0,0) (acc (rows grid) (cols grid)) grid else error "Invalid grid!"

validGrid :: Grid -> Bool
validGrid Grid { cells=cs, cols=c, rows=r} = c == length (cs!!0) &&
                                             r == length cs &&
                                             (fmap (length) cs) |> (\xs -> length (nub xs) == 1)

nextGrid :: (Int, Int) -> Grid -> Grid -> Grid
nextGrid (row,col) result grid
  | inc row == (rows grid) && inc col > (cols grid) = result
  | inc col > (cols grid) = nextGrid (inc row, 0) result grid
  | inc row > (rows grid) = nextGrid (0, inc col) result grid
  | otherwise = (nextCell (row,col) grid) |> \newCell -> nextGrid (row, inc col) (appendCell newCell result) grid


nextCell :: (Int, Int) -> Grid -> Cell
nextCell (r,c) grid  | cell (r,c) grid == Alive && (numNeighbours Alive (r,c) grid) < 2 = Dead
                     | cell (r,c) grid == Alive && (numNeighbours Alive (r,c) grid) > 3 = Dead
                     | cell (r,c) grid == Dead && (numNeighbours Alive (r,c) grid) == 3 = Alive
                     | otherwise = cell (r,c) grid

cell (r,c) grid = (cells grid)!!r!!c

numNeighbours :: Cell -> (Int, Int) -> Grid -> Int
numNeighbours state (r,c) grid = numTimesFound state neighbourCells
  where numTimesFound x xs = (length . filter (== x)) xs
        neighbourCells = filterOutLayers all |> (\xs -> fmap (\pos -> cell pos grid) xs)
        all = [(dec r, dec c),(dec r, c),(dec r, inc c),(r, dec c),(r, inc c),(inc r, dec c),(inc r, c),(inc r, inc c)]
        filterOutLayers xs = filter (\(r,c) -> r >=0 && c >= 0 && r < (rows grid) && c < (cols grid)) xs

appendCell :: Cell -> Grid -> Grid
appendCell cell grid@Grid{ cells = [] } = grid { cells = [[cell]]}
appendCell cell grid = grid { cells = newCells}
  where newCells = if length (last (cells grid)) < (cols grid) then dropLast ++ [newLast] else  (cells grid)  ++ [[cell]]
        newLast = (head (reverse  (cells grid))) ++ [cell]
        dropLast = reverse (tail (reverse  (cells grid)))

inc a = a + 1

dec a = a - 1

(|>) = flip ($)
