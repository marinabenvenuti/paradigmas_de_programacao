
import Board
import Solver
 
main :: IO ()
main = do
  putStrLn "=== Skyscrapers ==="
  putStrLn "\nInitial Board:\n"
  printInnerGrid (getInnerGrid board9909986)
  putStrLn "\nSolving...\n"
  -- fillObviousNs pré-preenche células óbvias antes de iniciar o backtracking,
  case solve (fillObviousNs board9909986) of
    Nothing     -> putStrLn "No solution was found."
    Just solved -> do
      printInnerGrid (getInnerGrid solved)
