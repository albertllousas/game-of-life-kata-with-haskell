module GameOfLifeSpec where

import Test.Hspec
import Control.Monad.Cont
import GameOfLife

gridOf xs = Grid{ cells = xs, cols= length(xs!!0), rows = length xs}

a = Alive
d = Dead

spec :: Spec
spec = do

  describe "Game" $ do

    it "Should calculate the next generation of Conway’s game of life" $ do
      let grid = gridOf [[d,d,d,d,d,d,d,d],
                         [d,d,d,d,a,d,d,d],
                         [d,d,d,a,a,d,d,d],
                         [d,d,d,d,d,d,d,d]]

      nextGeneration grid `shouldBe` gridOf [[d,d,d,d,d,d,d,d],
                                             [d,d,d,a,a,d,d,d],
                                             [d,d,d,a,a,d,d,d],
                                             [d,d,d,d,d,d,d,d]]

  describe "Game of life rules" $ do

    forM_ [
           (gridOf [[a,d],
                    [d,d]], (0,0), Dead),
           (gridOf [[d,a],
                    [d,d]], (0,1), Dead),
           (gridOf [[d,d],
                    [a,d]], (1,0), Dead),
           (gridOf [[d,d],
                    [d,a]], (1,1), Dead),
           (gridOf [[d,d,d],
                    [d,a,d],
                    [d,d,d]], (1,1), Dead)
           ] $ \(grid, pos, state) ->
      it ("Any live cell with fewer than two live neighbours dies. Case for "++(show grid)++" next "++(show pos)++" is "++(show state)) $ do
        nextCell pos grid `shouldBe` state

    forM_ [
           (gridOf [[d,d,d],
                    [d,a,a],
                    [a,a,a]], (2,1), Dead),
           (gridOf [[d,d,d],
                    [a,a,d],
                    [a,a,d]], (2,0), Alive),
           (gridOf [[a,d,a],
                    [d,a,d],
                    [a,d,a]], (1,1), Dead)
           ] $ \(grid, pos, state) ->
      it ("Any live cell with more than three live neighbours dies. Case for "++(show grid)++" next "++(show pos)++" is "++(show state)) $ do
        nextCell pos grid `shouldBe` state

    forM_ [
             (gridOf [[d,a,d],
                      [a,a,a],
                      [a,a,a]], (0,0), Alive),
             (gridOf [[d,a,d],
                      [a,d,a],
                      [d,a,d]], (1,1), Dead),
             (gridOf [[a,d,a],
                      [d,a,d],
                      [a,d,a]], (2,1), Alive)
            ] $ \(grid, pos, state) ->
      it ("Any dead cell with exactly three live neighbours becomes a live cell. Case for "++(show grid)++" next "++(show pos)++" is "++(show state)) $ do
        nextCell pos grid `shouldBe` state

  describe "Working with grids" $ do

    forM_ [
           (Grid{ cells=[], rows=2, cols=2 } , Dead, Grid{ cells=[[Dead]], rows=2, cols=2 }),
           (Grid{ cells=[[Dead]], rows=3, cols=2 } , Dead, Grid{ cells=[[Dead, Dead]], rows=3, cols=2 }),
           (Grid{ cells=[[Dead, Alive]], rows=3, cols=2 } , Alive, Grid{ cells=[[Dead, Alive], [Alive]], rows=3, cols=2 })
           ] $ \(grid, cell2add, newGrid) ->
      it ("Should append "++(show cell2add)++" cell to "++(show grid)++" with an expected result of "++(show newGrid)) $ do
        appendCell cell2add grid `shouldBe` newGrid

    forM_ [
           (gridOf [[a,d],
                    [d,d]], (0,0), Dead, 3),
           (gridOf [[a,d],
                    [d,d]], (0,0), Alive, 0),
           (gridOf [[a,a],
                    [d,d]], (0,0), Alive, 1),
           (gridOf [[a,a],
                    [d,d]], (0,1), Alive, 1),
           (gridOf [[a,a],
                    [d,d],
                    [d,d]], (1,0), Dead, 3),
           (gridOf [[a,a,a],
                    [a,a,a],
                    [a,a,a]], (1,1), Alive, 8),
           (gridOf [[d,a,d],
                    [a,a,a],
                    [d,a,d]], (1,1), Alive, 4),
           (gridOf [[d,a,d],
                    [a,a,a],
                    [d,a,d]], (2,2), Alive, 3),
           (gridOf [[d,d,d],
                    [d,a,a],
                    [a,a,a]], (2,1), Alive, 4)
            ] $ \(grid, pos, state, result) ->
      it ("Should count how many "++(show state)++" cells has "++(show grid)++" at "++(show pos)++" with an expected result of "++(show result)) $ do
        numNeighbours state pos grid `shouldBe` result