module Solver where

import Board
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List  (transpose)

-- Conta quantos arranha-céus são visíveis olhando da esquerda para a direita.
-- Um prédio é visível se for mais alto que todos os anteriores.
-- O fold carrega (maiorVisto, contadorVisíveis) e atualiza os dois a cada célula.
countVisible :: [Cell] -> Int
countVisible cells =
  let (_, result) = foldl compareTallest (0, 0) cells
  in  result
  where
    compareTallest (currentTallest, acc) cell
      | cell > currentTallest = (cell, acc + 1)
      | otherwise             = (currentTallest, acc)

-- Verifica se o valor `v` pode ser colocado em (i, j):
-- não pode repetir na linha nem na coluna.
isSafe :: Board -> Int -> Int -> Int -> Bool
isSafe board i j v =
  notElem v (getBoardLine board i) &&
  notElem v (getColumn board j)

-- As quatro funções abaixo são otimizações de pré-preenchimento:
-- quando uma pista vale 1, o prédio mais alto (N) deve estar na
-- primeira célula visível daquela direção, pois bloqueia todos os outros.

fillObviousNsLeft :: Board -> Board
fillObviousNsLeft (Board innerGrid clues) =
  case getIndex (left clues) 1 of
    Nothing -> (Board innerGrid clues)
    -- pista esquerda = 1 na linha i → coluna 0 recebe N
    Just i  -> setBoard (Board innerGrid clues) i 0 (length innerGrid)

fillObviousNsRight :: Board -> Board
fillObviousNsRight (Board innerGrid clues) =
  case getIndex (right clues) 1 of
    Nothing -> (Board innerGrid clues)
    -- pista direita = 1 na linha i → última coluna recebe N
    Just i  -> setBoard (Board innerGrid clues) i ((length innerGrid) - 1) (length innerGrid)

fillObviousNsTop :: Board -> Board
fillObviousNsTop (Board innerGrid clues) =
  case getIndex (top clues) 1 of
    Nothing -> (Board innerGrid clues)
    -- pista topo = 1 na coluna j → linha 0 recebe N
    Just j  -> setBoard (Board innerGrid clues) 0 j (length innerGrid)

fillObviousNsBottom :: Board -> Board
fillObviousNsBottom (Board innerGrid clues) =
  case getIndex (bottom clues) 1 of
    Nothing -> (Board innerGrid clues)
    -- pista base = 1 na coluna j → última linha recebe N
    Just j  -> setBoard (Board innerGrid clues) ((length innerGrid) - 1) j (length innerGrid)

-- Aplica os quatro pré-preenchimentos em sequência (composição de funções).
fillObviousNs :: Board -> Board
fillObviousNs board =
  fillObviousNsBottom (fillObviousNsTop (fillObviousNsRight (fillObviousNsLeft board)))

-- Verifica se uma linha/coluna é compatível com sua pista:
-- • Se a linha está completa (sem zeros), a contagem deve ser EXATAMENTE igual à pista.
-- • Se ainda há zeros (linha parcial), a contagem dos prédios já preenchidos
--   não pode SUPERAR a pista — podar ramos inválidos cedo no backtracking.
isConsistent :: [Cell] -> Int -> Bool -> Bool
isConsistent cells clue isComplete
  | isComplete = countVisible cells == clue
  | otherwise  = countVisible (takeWhile (/= 0) cells) <= clue

-- As quatro funções de consistência seguem o mesmo padrão:
-- zip emparelha cada linha/coluna com sua pista correspondente e
-- `all` exige que todos os pares satisfaçam checkLine.

isLeftConsistent :: Board -> Bool
isLeftConsistent (Board innerGrid clues) =
  all (\(line, clue) -> checkLine line clue) (zip innerGrid (left clues))

isRightConsistent :: Board -> Bool
isRightConsistent (Board innerGrid clues) =
  -- reverse inverte a linha para contar visibilidade da direita
  all (\(line, clue) -> checkLine (reverse line) clue) (zip innerGrid (right clues))

-- transpose converte a matriz de linhas em matriz de colunas,
-- permitindo reutilizar a mesma lógica de verificação horizontal.
isTopConsistent :: Board -> Bool
isTopConsistent (Board innerGrid clues) =
  all (\(column, clue) -> checkLine column clue) (zip (transpose innerGrid) (top clues))

isBottomConsistent :: Board -> Bool
isBottomConsistent (Board innerGrid clues) =
  -- reverse inverte a coluna para contar visibilidade de baixo para cima
  all (\(column, clue) -> checkLine (reverse column) clue) (zip (transpose innerGrid) (bottom clues))

checkLine :: [Cell] -> Int -> Bool
checkLine cells clue =
  let isComplete = notElem 0 cells
  in  isConsistent cells clue isComplete

isBoardConsistent :: Board -> Bool
isBoardConsistent board =
  isLeftConsistent  board &&
  isRightConsistent board &&
  isTopConsistent   board &&
  isBottomConsistent board

-- `all (notElem 0)` aproveita currying: notElem 0 é uma função [Cell] -> Bool
-- aplicada a cada linha pelo `all`.
isBoardSolved :: Board -> Bool
isBoardSolved board = all (notElem 0) (getInnerGrid board)

-- Busca a primeira célula com valor 0 (vazia) varrendo linha por linha.
-- Retorna (i, j) ou Nothing se não houver célula vazia.
findEmpty :: [[Cell]] -> Int -> Maybe (Int, Int)
findEmpty []    _ = Nothing
findEmpty (h:t) i =
  case (getIndex h 0) of
    Just j  -> Just (i, j)
    Nothing -> findEmpty t (i + 1)

tryValue :: Board -> Int -> Int -> Int -> Board
tryValue board i j v = setBoard board i j v

-- Solver por backtracking:
-- 1. Se o tabuleiro está resolvido, retorna Just board.
-- 2. Acha a próxima célula vazia (i, j).
-- 3. Filtra os valores 1..N que são seguros para (i, j).
-- 4. Gera os tabuleiros candidatos e descarta os inconsistentes com as pistas.
-- 5. Tenta resolver cada candidato recursivamente.
--    `mapMaybe` descarta os Nothing (ramos sem solução) e
--    `listToMaybe` retorna o primeiro Just encontrado (primeira solução válida).
solve :: Board -> Maybe Board
solve board
  | isBoardSolved board = Just board
  | otherwise =
      case findEmpty (getInnerGrid board) 0 of
        Nothing     -> Nothing
        Just (i, j) ->
          let validValues = filter (isSafe board i j) [1..(getOrder board)]
              nextBoards  = map (tryValue board i j) validValues
          in  listToMaybe (mapMaybe solve (filter isBoardConsistent nextBoards))