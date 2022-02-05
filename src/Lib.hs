{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}

module Lib
  ( simulation,
  )
where

import System.IO.Unsafe
import System.Random

data Accion = Move | Drop | Clean | Pass deriving (Eq, Show)

data Cell
  = Obstacle
  | Child
  | Dirt
  | Playpen {cells :: [Cell]}
  | Bot {cells :: [Cell]}
  | Empty
  deriving (Show, Eq)

data Dir = N | E | S | O deriving (Show, Eq)

tupleToDir :: (Int, Int) -> Dir
tupleToDir dir
  | dir == (0, 1) = E
  | dir == (0, -1) = O
  | dir == (1, 0) = S
  | dir == (-1, 0) = N

dirToTuple :: Dir -> (Int, Int)
dirToTuple dir
  | dir == E = (0, 1)
  | dir == O = (0, -1)
  | dir == S = (1, 0)
  | dir == N = (-1, 0)

getNewPos :: Dir -> (Int, Int) -> (Int, Int)
getNewPos dir coorO =
  let (x, y) = dirToTuple dir
   in (x + fst coorO, y + snd coorO)

initEnvironment :: Int -> Int -> ([[Cell]], StdGen)
initEnvironment n m =
  let gen = mkStdGen 12323
      b1 = createBoard n m
      (b2, gen1) = fillBoard gen b1
   in (b2, gen1)

createBoard :: Int -> Int -> [[Cell]]
createBoard n m =
  let b = [[Empty | y <- [0 .. m - 1]] | x <- [0 .. n - 1]]
   in b

fillBoard :: StdGen -> [[Cell]] -> ([[Cell]], StdGen)
fillBoard gen board =
  let (n, m) = getLen board
      _min = min n m

      (childs, gen1) = getRandom gen 1 _min
      (bots, gen2) = getRandom gen1 1 _min
      (dirts, gen3) = getRandom gen2 1 _min
      (obstacles, gen4) = getRandom gen3 1 _min

      (b, gen12) = createPlaypen gen board childs childs (0, 0)
      (b1, gen13) = generateRandomCells gen12 Child b childs
      (b2, gen14) = generateRandomCells gen12 Bot {cells = []} b1 bots
      (b3, gen15) = generateRandomCells gen14 Dirt b2 dirts
      (b4, gen16) = generateRandomCells gen15 Obstacle b3 obstacles
   in (b4, gen16)

createPlaypen :: StdGen -> [[Cell]] -> Int -> Int -> (Int, Int) -> ([[Cell]], StdGen)
createPlaypen gen board k count coor
  | k == count = createPlaypen gen (insertCell Playpen {cells = []} coor board) k (count -1) coor
  | count == 0 = (board, gen)
  | otherwise =
    let (b, t, gen1) = insertPlaypent gen board coor
     in createPlaypen gen1 b k (count -1) t

insertPlaypent :: StdGen -> [[Cell]] -> (Int, Int) -> ([[Cell]], (Int, Int), StdGen)
insertPlaypent gen board coor =
  let neighbors = [(x, y) | (x, y, cell) <- getNeighbors coor board, cell == Empty]
      len = length neighbors
      (r, gen1) = getRandom gen 0 (len -1)
      t = neighbors !! r
   in (insertCell Playpen {cells = []} t board, t, gen1)

removeCell :: (Int, Int) -> [[Cell]] -> [[Cell]]
removeCell coor board =
  let (n, m) = getLen board
      b = [[if fst coor == x && snd coor == y then Empty else indexOf (x, y) board | y <- [0 .. m -1]] | x <- [0 .. n -1]]
   in b

insertCell :: Cell -> (Int, Int) -> [[Cell]] -> [[Cell]]
insertCell cell coor board =
  let (n, m) = getLen board
      b = [[if fst coor == x && snd coor == y then cell else indexOf (x, y) board | y <- [0 .. m -1]] | x <- [0 .. n -1]]
   in b

indexOf :: (Int, Int) -> [[Cell]] -> Cell
indexOf coor board =
  let element = (board !! fst coor) !! snd coor
   in element

getRandom :: StdGen -> Int -> Int -> (Int, StdGen)
getRandom gen a b = randomR (a, b) gen :: (Int, StdGen)

getRandomTuple :: StdGen -> [[Cell]] -> ((Int, Int), StdGen)
getRandomTuple gen board =
  let (n, m) = getLen board
      (f, gen1) = getRandom gen 0 (n -1)
      (s, gen2) = getRandom gen1 0 (m -1)
   in if indexOf (f, s) board == Empty then ((f, s), gen2) else getRandomTuple gen2 board

generateRandomSet :: StdGen -> Int -> [[Cell]] -> [(Int, Int)] -> ([(Int, Int)], StdGen)
generateRandomSet gen 0 board set = (set, gen)
generateRandomSet gen k board set =
  let (t, gen1) = getRandomTuple gen board
   in if t `elem` set then generateRandomSet gen1 k board set else generateRandomSet gen1 (k -1) board (t : set)

insertCells :: Cell -> [(Int, Int)] -> [[Cell]] -> [[Cell]]
insertCells _ [] board = board
insertCells cell (t : xs) board = insertCells cell xs (insertCell cell t board)

generateRandomCells :: StdGen -> Cell -> [[Cell]] -> Int -> ([[Cell]], StdGen)
generateRandomCells gen cell board k =
  let (ts, gen1) = generateRandomSet gen k board []
   in (insertCells cell ts board, gen1)

bfs :: [Cell] -> [(Int, Int)] -> [[Cell]] -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> [((Int, Int), Int)] -> ([((Int, Int), (Int, Int))], [((Int, Int), Int)])
bfs _ [] _ _ pi ds = (pi, ds)
bfs filters (u : q) board mark pi ds =
  let neighbors = getNeighbors u board
      neighbors1 = [(x, y, value) | (x, y, value) <- neighbors, (x, y) `notElem` mark, indexOf (x, y) board `elem` filters]
      d' = head [v | ((x, y), v) <- ds, (x, y) == u]
      ds' = ds ++ [((x, y), d' + 1) | (x, y, _) <- neighbors1]
      mark' = mark ++ [(x, y) | (x, y, _) <- neighbors1]
      q' = q ++ [(x, y) | (x, y, _) <- neighbors1]
      pi' = pi ++ [(u, (x, y)) | (x, y, _) <- neighbors1]
   in bfs filters q' board mark' pi' ds'

getBestPos :: [((Int, Int), (Int, Int))] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getBestPos pi coorO coorD
  | coorO == coorD = coorO
  | otherwise =
    let father = head [x | (x, y) <- pi, y == coorD]
        result
          | father == coorO = coorD
          | otherwise = getBestPos pi coorO father
     in result

getNearestElement :: String -> [Cell] -> [[Cell]] -> (Int, Int) -> ((Int, Int), [((Int, Int), (Int, Int))])
getNearestElement eType filters board coor =
  let (pi, ds) = bfs filters [coor] board [coor] [] [(coor, 0)]
      tmp = [c | (c, v) <- ds, cellType (indexOf c board) == eType]
      nearest
        | null tmp = coor
        | otherwise = head tmp
   in (nearest, pi)

isIn :: (Int, Int) -> (Int, Int) -> Bool
isIn (x, y) (n, m)
  | x < 0 || x >= n || y < 0 || y >= m = False
  | otherwise = True

isInEdge :: Dir -> (Int, Int) -> (Int, Int) -> Bool
isInEdge dir (x, y) (n, m)
  | dir == N = x == 0
  | dir == S = x == (n -1)
  | dir == E = y == (m -1)
  | dir == O = y == 0

getNeighbors :: (Int, Int) -> [[Cell]] -> [(Int, Int, Cell)]
getNeighbors coor board
  | not (isIn coor (n, m)) = error "index out of range"
  | otherwise =
    [ (r + fst d, c + snd d, indexOf (r + fst d, c + snd d) board)
      | d <- zip [1, -1, 0, 0] [0, 0, 1, -1],
        isIn (r + fst d, c + snd d) (n, m)
    ]
  where
    (n, m) = getLen board
    (r, c) = coor

getNeighbors' :: (Int, Int) -> [[Cell]] -> [(Int, Int, Cell)]
getNeighbors' coor board
  | not (isIn coor (n, m)) = error "index out of range"
  | otherwise =
    [ (r + fst d, c + snd d, indexOf (r + fst d, c + snd d) board)
      | d <- zip [-1, -1, -1, 1, 1, 1, 0, 0] [-1, 1, 0, -1, 1, 0, -1, 1],
        isIn (r + fst d, c + snd d) (n, m)
    ]
  where
    (n, m) = getLen board
    (r, c) = coor

getLen :: [[Cell]] -> (Int, Int)
getLen board =
  let n = length board
      m = length (head board)
   in (n, m)

cellToChar :: Cell -> String
cellToChar Empty = "_"
cellToChar Child = "C"
cellToChar Obstacle = "O"
cellToChar Dirt = "D"
cellToChar Bot {cells = []} = "B"
cellToChar Bot {cells = [Child]} = "BC"
cellToChar Bot {cells = [Dirt]} = "BD"
cellToChar Bot {cells = [_, _]} = "BCD"
cellToChar Playpen {cells = []} = "P"
cellToChar Playpen {cells = [Child]} = "PC"
cellToChar Playpen {cells = [Bot {cells = []}]} = "PB"
cellToChar Playpen {cells = [Bot {cells = [Child]}]} = "PBC"
cellToChar Playpen {cells = [Bot {cells = []}, Child]} = "PB&C"

cellType :: Cell -> String
cellType Empty = "Empty"
cellType Child = "Child"
cellType Obstacle = "Obstacle"
cellType Dirt = "Dirt"
cellType Bot {cells = _} = "Bot"
cellType Playpen {cells = _} = "Playpent"

printRow :: [Cell] -> [String]
printRow = map cellToChar

printBoard :: [[Cell]] -> Int -> IO ()
printBoard board (-1) = putStrLn ""
printBoard board n = do
  print (printRow (board !! (length board - n - 1)))
  printBoard board (n -1)

removeElement :: [Cell] -> Cell -> [Cell]
removeElement cells cell = [c | c <- cells, c /= cell]

dropChild :: (Int, Int) -> [[Cell]] -> [[Cell]]
dropChild cell board =
  let p = indexOf cell board
      bot = head (cells p)
   in insertCell Playpen {cells = [Bot {cells = []}, Child]} cell board

moveRobot :: Cell -> [[Cell]] -> (Int, Int) -> Dir -> [[Cell]]
moveRobot bot board coorO dir
  | coorO == coorD = board
  | isInPlaypent && isPlaypent =
    let isChild = Child `elem` cells (indexOf coorO board)
        b1 = insertCell (if isChild then Playpen {cells = [Child]} else Playpen {cells = []}) coorO board

        newCells = if element == Playpen {cells = []} then _cells else Child : _cells
     in insertCell Playpen {cells = [Bot {cells = newCells}]} coorD b1
  | isInPlaypent =
    let isChild = Child `elem` cells (indexOf coorO board)
        b1 = insertCell (if isChild then Playpen {cells = [Child]} else Playpen {cells = []}) coorO board

        newCells = if element == Empty then _cells else element : _cells
     in insertCell Bot {cells = newCells} coorD b1
  | isPlaypent =
    let dirt = Dirt `elem` _cells
        removedDirt = removeElement _cells Dirt
        newCell
          | element == Playpen {cells = []} = if dirt then Bot {cells = removedDirt} else Bot {cells = _cells}
          | otherwise = if dirt then Bot {cells = Child : removedDirt} else Bot {cells = Child : _cells}

        b1 = if dirt then insertCell Dirt coorO board else insertCell Empty coorO board
     in insertCell Playpen {cells = [newCell]} coorD b1
  | Dirt `notElem` _cells =
    let newCells = if element == Empty then _cells else element : _cells
     in insertCell Bot {cells = newCells} coorD (removeCell coorO board)
  | otherwise =
    let removedDirt = removeElement _cells Dirt
        newCells = if element == Empty then removedDirt else element : removedDirt
     in insertCell Bot {cells = newCells} coorD (insertCell Dirt coorO (removeCell coorO board))
  where
    (isInPlaypent, _cells, coorD, element, isPlaypent) = (bot `elem` cells (indexOf coorO board), cells bot, getNewPos dir coorO, indexOf coorD board, cellType element == "Playpent")

clean :: [[Cell]] -> (Int, Int) -> [[Cell]]
clean board coor =
  let bot = indexOf coor board
      newCells = removeElement (cells bot) Dirt
   in insertCell bot {cells = newCells} coor board

getLastObstacle :: [[Cell]] -> (Int, Int) -> Dir -> (Int, Int)
getLastObstacle board coorO dir =
  let (x, y) = dirToTuple dir
      newCoor = (fst coorO + x, snd coorO + y)
      isin = isIn newCoor (n, m)
   in if isin && indexOf newCoor board == Obstacle then getLastObstacle board newCoor dir else coorO
  where
    (n, m) = getLen board

pushObstacles :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Dir -> [[Cell]]
pushObstacles board coorO last dir =
  let isValid
        | isInEdge dir last (n, m) = False
        | indexOf newLast board /= Empty = False
        | otherwise = True

      b1
        | isValid = insertCell Obstacle newLast board
        | otherwise = board

      b2
        | isValid = insertCell Child current (removeCell coorO b1)
        | otherwise = board
   in b2
  where
    (x, y) = dirToTuple dir
    (n, m) = getLen board
    (newLast, current) = ((fst last + x, snd last + y), (fst coorO + x, snd coorO + y))

childMove :: [[Cell]] -> (Int, Int) -> Dir -> [[Cell]]
childMove board coorO dir
  | element == Empty = insertCell Child newPos (removeCell coorO board)
  | element == Obstacle =
    let (n, m) = getLen board
        last = getLastObstacle board newPos dir
     in pushObstacles board coorO last dir
  | otherwise = board
  where
    (newPos, element) = (getNewPos dir coorO, indexOf newPos board)

childDirt :: StdGen -> [[Cell]] -> (Int, Int) -> ([[Cell]], StdGen)
childDirt gen board coor =
  let neighbors = getNeighbors' coor board
      empty = if null neighbors then [] else [(x, y) | (x, y, t) <- neighbors, t == Empty]

      (b, g)
        | null empty = (board, gen)
        | otherwise =
          let len = length empty
              (r, gen1) = getRandom gen 0 (len -1)
              (x, y) = empty !! r

              (r1, gen2) = getRandom gen1 0 5
              put
                | r1 == 1 = insertCell Dirt (x, y) board
                | otherwise = board
           in (put, gen2)
   in (b, g)

simulateChildMove :: StdGen -> [[Cell]] -> (Int, Int) -> ([[Cell]], StdGen)
simulateChildMove gen board coor =
  let neighbors = [(x, y) | (x, y, cell) <- getNeighbors coor board, cell == Empty || cell == Obstacle]
      b
        | null neighbors = (board, gen)
        | otherwise =
          let len = length neighbors
              (r, gen1) = getRandom gen 0 (len -1)
              (x, y) = neighbors !! r
              tmp = (x - fst coor, y - snd coor)
              (move, gen2) = (childMove board coor (tupleToDir tmp), gen1)
           in childDirt gen2 move (x, y)
   in b

getAllChild :: [[Cell]] -> [(Int, Int)]
getAllChild board =
  let (n, m) = getLen board
      tmp = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)]]
      childs = filter f tmp
        where
          f (x, y) = indexOf (x, y) board == Child
   in childs

getAllBot :: [[Cell]] -> [(Int, Int)]
getAllBot board =
  let (n, m) = getLen board
      tmp = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)]]
      bots = filter f tmp
        where
          f (x, y) = cellType (indexOf (x, y) board) == "Bot"

      playpents = filter f tmp
        where
          f (x, y) = cellType (indexOf (x, y) board) == "Playpent"

      botP = filter f playpents
        where
          f (x, y)
            | null (cells p) = False
            | otherwise = cellType (head (cells p)) == "Bot"
            where
              p = indexOf (x, y) board
   in bots ++ botP

getAllDirt :: [[Cell]] -> [(Int, Int)]
getAllDirt board =
  let (n, m) = getLen board
      tmp = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)]]
      childs = filter f tmp
        where
          f (x, y) = indexOf (x, y) board == Dirt
   in childs

getBot :: [[Cell]] -> (Int, Int) -> Cell
getBot board coor
  | cellType element == "Bot" = element
  | otherwise = head (cells element)
  where
    element = indexOf coor board

getBotPosMove :: [[Cell]] -> (Int, Int) -> [(Int, Int)]
getBotPosMove board coor =
  let bot = getBot board coor

      types1 = [Empty, Dirt, Playpen {cells = []}]
      types2 = [Empty, Dirt, Child, Playpen {cells = []}, Playpen {cells = [Child]}]

      neighbors
        | Child `elem` cells bot = [(x, y) | (x, y, cell) <- getNeighbors coor board, cell `elem` types1]
        | otherwise = [(x, y) | (x, y, cell) <- getNeighbors coor board, cell `elem` types2]
   in neighbors

getBotPosAccions :: [[Cell]] -> (Int, Int) -> [Accion]
getBotPosAccions board coor =
  let bot = getBot board coor
      moves = getBotPosMove board coor

      accions
        | Child `notElem` cells bot && Dirt `notElem` cells bot && not (null moves) = [Move]
        | Child `notElem` cells bot && Dirt `elem` cells bot && not (null moves) = [Move, Clean]
        | Child `notElem` cells bot && Dirt `elem` cells bot = [Clean]
        | Child `elem` cells bot && cType == "Playpent" && length moves > 0 = [Move, Drop]
        | Child `elem` cells bot && cType == "Playpent" = [Drop]
        | Child `elem` cells bot && length moves > 0 = [Move]
        | otherwise = [Pass]
        where
          cType = cellType (indexOf coor board)
   in accions

getBestMove :: Int -> [[Cell]] -> (Int, Int) -> (Int, Int)
getBestMove botType board coor
  | botType == 0 =
    let filters = [Dirt, Empty, Playpen {cells = []}]
        (n, pi) = getNearestElement "Dirt" filters board coor
     in getBestPos pi coor n
  | botType == 1 =
    let bot = getBot board coor
        filters = if Child `elem` cells bot then [Empty, Dirt, Playpen {cells = []}] else [Empty, Dirt, Child, Playpen {cells = []}]
        childs = getAllChild board
        eType = if null childs then "Dirt" else "Child"

        (n, pi)
          | Child `elem` cells bot = getNearestElement "Playpent" filters board coor
          | null childs = getNearestElement "Dirt" filters board coor
          | otherwise = getNearestElement "Child" filters board coor
     in getBestPos pi coor n

getBestAccion :: Int -> [[Cell]] -> (Int, Int) -> Accion
getBestAccion botType board coor
  | botType == 0 =
    let accions = getBotPosAccions board coor
        dirts = getAllDirt board
        accion
          | Clean `elem` accions = Clean
          | null dirts = Pass
          | Move `elem` accions = Move
          | otherwise = Pass
     in accion
  | botType == 1 =
    let accions = getBotPosAccions board coor
        childs = getAllChild board
        dirts = getAllDirt board
        accion
          | Drop `elem` accions = Drop
          | null childs && Clean `elem` accions = Clean
          | null childs && not (null dirts) = Move
          | null dirts = Pass
          | Move `elem` accions = Move
          | otherwise = Pass
     in accion

simulateAccion :: Int -> [[Cell]] -> Accion -> (Int, Int) -> [[Cell]]
simulateAccion botType board accion coor =
  let bot = getBot board coor
      b
        | accion == Pass = board
        | accion == Drop = dropChild coor board
        | accion == Clean = clean board coor
        | accion == Move =
          let best = getBestMove botType board coor
              tmp = (fst best - fst coor, snd best - snd coor)
           in if coor == best then board else moveRobot bot board coor (tupleToDir tmp)
   in b

simulateRobot :: Int -> [[Cell]] -> (Int, Int) -> [[Cell]]
simulateRobot botType board coor =
  let accion = getBestAccion botType board coor
   in simulateAccion botType board accion coor

simulateChilds :: StdGen -> [[Cell]] -> [(Int, Int)] -> ([[Cell]], StdGen)
simulateChilds gen board [] = (board, gen)
simulateChilds gen board (x : xs) =
  let (b1, gen1) = simulateChildMove gen board x
   in simulateChilds gen1 b1 xs

simulateRobots :: Int -> [[Cell]] -> [(Int, Int)] -> [[Cell]]
simulateRobots botType board [] = board
simulateRobots botType board (x : xs) =
  let b1 = simulateRobot botType board x
   in simulateRobots botType b1 xs

changeEnvironment :: StdGen -> [[Cell]] -> ([[Cell]], StdGen)
changeEnvironment gen board =
  let voids = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)], indexOf (x, y) board == Empty]
   in putDirt gen voids board
  where
    (n, m) = getLen board

putDirt :: StdGen -> [(Int, Int)] -> [[Cell]] -> ([[Cell]], StdGen)
putDirt gen [] board = (board, gen)
putDirt gen (x : xs) board =
  let (r, gen1) = getRandom gen 0 5
      board' = if r == 1 then insertCell Dirt x board else board
   in putDirt gen1 xs board'

getPercent :: [[Cell]] -> Float
getPercent board =
  let voids = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)], indexOf (x, y) board == Empty]
      dirts = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)], indexOf (x, y) board == Dirt]
      lenVoids = length voids
      lenDirt = length dirts
   in percent lenVoids (lenDirt + lenVoids)
  where
    (n, m) = getLen board

percent :: Int -> Int -> Float
percent x y = 100 * (a / b)
  where
    a = fromIntegral x :: Float
    b = fromIntegral y :: Float

simulate :: StdGen -> Int -> [[Cell]] -> Int -> Int -> ([[Cell]], StdGen)
simulate gen botType board t 0 = (board, gen)
simulate gen botType board t k =
  let m = mod k t
      (b, g) = if m == 0 then changeEnvironment gen board else (board, gen)
      (bc, gc) = simulateChilds g b (getAllChild board)
      bb = simulateRobots botType bc (getAllBot board)
   in simulate gc botType bb t (k -1)

simulation :: IO ()
simulation =
  let (board, gen) = initEnvironment 20 20
      (b0, g0) = simulate gen 0 board 10 300
      p0 = getPercent b0
      (b1, g1) = simulate gen 1 board 10 300
      p1 = getPercent b1
   in do
        printBoard board 19
        printBoard b0 19
        print p0
        printBoard b1 19
        print p1