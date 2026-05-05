module Board where

data Board = Board [[Cell]] Clues
  deriving (Show)

data Clues = Clues
  { top    :: [Int],
    bottom :: [Int],
    left   :: [Int],
    right  :: [Int]
  }
  deriving (Show)

-- Cell representa a altura de um arranha-céu (0 = célula vazia)
type Cell = Int

getBoardLine :: Board -> Int -> [Cell]
getBoardLine (Board board _) index = board !! index

getColumn :: Board -> Int -> [Cell]
getColumn board j = map (\i -> getCell (getBoardLine board i) j) [0..n-1]
  where n = getOrder board

getCell :: [Cell] -> Int -> Cell
getCell line index = line !! index

getClues :: Board -> Clues
getClues (Board _ clues) = clues

getClue :: [Int] -> Int -> Int
getClue clues index = clues !! index

-- Retorna o índice da primeira ocorrência de `value` na lista,
-- ou Nothing se não encontrar. Reconstrói o índice na volta da recursão (Just (i+1))
-- para não precisar de um acumulador explícito.
getIndex :: [Int] -> Int -> Maybe Int
getIndex [] _ = Nothing
getIndex (h:t) value
  | value == h = Just 0
  | otherwise  =
      case (getIndex t value) of
        Nothing -> Nothing
        Just i  -> Just (i + 1)

-- Conta linhas recursivamente em vez de usar `length`, para evitar
-- depender de funções de Prelude além do necessário.
getOrder :: Board -> Int
getOrder (Board [] _)      = 0
getOrder (Board (_:t) clues) = (getOrder (Board t clues)) + 1

getInnerGrid :: Board -> [[Cell]]
getInnerGrid (Board innerGrid _) = innerGrid

printInnerGrid :: Show a => [[a]] -> IO ()
printInnerGrid = mapM_ print

-- Substitui o elemento na posição `index` de uma lista de Cells.
-- Desce recursivamente decrementando o índice até chegar em 0,
-- então troca a cabeça pelo novo valor e reconstrói a lista na volta.
setCell :: [Cell] -> Int -> Cell -> [Cell]
setCell (_:t) 0     newValue = (newValue:t)
setCell (h:t) index newValue = (h : (setCell t (index - 1) newValue))

-- Percorre as linhas do tabuleiro recursivamente até a linha-alvo (i == 0),
-- aplica setCell nela e reconstrói o tabuleiro na volta da recursão.
-- É a única forma de "alterar" o tabuleiro imutável em Haskell.
setBoard :: Board -> Int -> Int -> Cell -> Board
setBoard (Board (h:t) clues) 0 j newValue =
  (Board ((setCell h j newValue) : t) clues)
setBoard (Board (h:t) clues) i j newValue =
  let (Board newMatrix _) = setBoard (Board t clues) (i - 1) j newValue
  in  (Board (h:newMatrix) clues)

-- Tabuleiro 4x4 zerado para teste
board8139 :: Board
board8139 = Board
  [ [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  , [0, 0, 0, 0]
  ] clues4x4

clues4x4 :: Clues
clues4x4 = Clues
  { top    = [2, 4, 2, 1]
  , bottom = [3, 1, 2, 3]
  , left   = [2, 1, 3, 2]
  , right  = [1, 2, 2, 3]
  }

-- Tabuleiros 6x6 zerados para teste
board9909986 :: Board
board9909986 = Board
  [ [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  ] clues9909986

clues9909986 :: Clues
clues9909986 = Clues
  { top    = [2, 4, 2, 5, 2, 1]
  , bottom = [2, 3, 3, 1, 2, 4]
  , left   = [3, 1, 3, 2, 4, 2]
  , right  = [1, 2, 3, 4, 2, 3]
  }

board9785244 :: Board
board9785244 = Board
  [ [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  ] clues9785244

clues9785244 :: Clues
clues9785244 = Clues
  { top    = [2, 2, 4, 3, 1, 2]
  , bottom = [3, 2, 1, 3, 4, 2]
  , left   = [3, 5, 4, 1, 2, 2]
  , right  = [2, 1, 3, 3, 3, 2]
  }

board6x6 :: Board
board6x6 = Board
  [ [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0]
  ] clues6x6

clues6x6 :: Clues
clues6x6 = Clues
  { top    = [4, 2, 1, 4, 3, 2]
  , bottom = [1, 2, 3, 3, 2, 3]
  , left   = [2, 3, 2, 2, 4, 1]
  , right  = [2, 1, 3, 2, 2, 5]
}