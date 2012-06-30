-- pieces of a Game of life implementation in Haskell
-- (just learning, this is rough)

-- instructions:
-- ghci
-- :l life.hs
-- let x = test            # generates the grid
-- let y = clockTick x     # evolves the grid 1 step
-- explainGrid x           # prints a grid iteration
-- explainGrid y           # " "

-- cell data structure

data Cell = Cell { xCoord :: Int
                 , yCoord :: Int
                 , alive :: Bool
                 } deriving (Show)

-- are two cells at the same coordinates
notsame :: Cell -> Cell -> Bool
notsame left right = ((xDist /= 0) || (yDist /= 0))
  where xDist = xdist left right
        yDist = ydist left right

-- how far apart are two cells on the x axis?
xdist :: Cell -> Cell -> Int
xdist left right = abs((xCoord left) - (xCoord right))

-- how far apart are two cells on the y axis?
ydist :: Cell -> Cell -> Int
ydist left right = abs((yCoord left) - (yCoord right))

-- find neighbors of the given cell
neighbors :: Cell -> [Cell] -> [Cell]
neighbors cell cells = [ other | other <- cells, (xdist cell other) <= 1,
                            (ydist cell other) <= 1,
                            notsame cell other ]

-- should the given cell be alive in the next iteration?
newAlive :: Cell -> [Cell] -> Bool
newAlive cell cells
  | nCt < 2 = False
  | nCt >= 2 && nCt <= 3 && ((alive cell) == True) = True
  | nCt == 3 && ((alive cell) == False) = True
  | nCt > 3 = False
  where nCt = length (neighbors cell cells)

-- compute the cell data item for the next iteration
nextCell :: Cell -> [Cell] -> Cell
nextCell cell cells = Cell { xCoord = xCoord cell, yCoord = yCoord cell, alive = newAlive cell cells } 

-- recompute all cells for the next iteration
clockTick :: [Cell] -> [Cell]
clockTick cells = [ nextCell cell cells | cell <- cells ]

-- return all cells on the given row
onRow :: Int -> [Cell] -> [Cell]
onRow num cells = [ foo | foo <- cells, xCoord foo == num ]

-- how wide is the gameboard on the x axis?
xDimension :: [Cell] -> Int
xDimension cells = maximum [ xCoord foo | foo <- cells ]

-- how wide is the gameboard on the y axis?
yDimension :: [Cell] -> Int
yDimension cells = maximum [ yCoord foo | foo <- cells ]

-- group cells by rows
byRows :: [Cell] -> [[Cell]]
byRows cells = [ onRow num cells | num <- [0 .. xDimension cells] ]

-- graphical representation of whether a cell is alive or dead
explainCell :: Cell -> Char
explainCell cell = if alive cell then 'X' else ' ' 

-- graphical representation of a row
explainCells :: [Cell] -> [Char]
explainCells row = [ explainCell foo | foo <- row ] ++ "\n"

-- graphical representation of a full playing field
explainGrid :: [Cell] -> [Char]
explainGrid cells = foldl (++) [] [ explainCells foo | foo <- byRows cells ]

-- possible additions:
-- a random generator function
-- loadFile, load a pattern from a file
-- module organization, use various imports
-- command line options and such so you don't need ghci
-- version using array libs?

test = [
         Cell { xCoord = 0, yCoord = 0, alive = True  },
         Cell { xCoord = 0, yCoord = 1, alive = True  },
         Cell { xCoord = 0, yCoord = 2, alive = True  },
         Cell { xCoord = 1, yCoord = 0, alive = False },
         Cell { xCoord = 1, yCoord = 1, alive = False },
         Cell { xCoord = 1, yCoord = 2, alive = True  },
         Cell { xCoord = 2, yCoord = 1, alive = False },
         Cell { xCoord = 2, yCoord = 2, alive = False }
       ]


