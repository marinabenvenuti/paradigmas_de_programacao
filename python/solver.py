from board import Board

def count_visible(line: list[int]) -> int:
    """
    conta quantos prédios são visíveis
    """
    visible = 0
    max_h = 0
    for h in line:
        if h > max_h:
            visible += 1
            max_h = h
    return visible


def is_line_valid(line: list[int], n: int) -> bool:
    """linha/coluna sem repetições e com valores em 1..n """
    filled = [x for x in line if x != 0]
    return len(filled) == len(set(filled)) and all(1 <= x <= n for x in filled)


def is_consistent(line: list[int], clue: int, complete: bool) -> bool:
    """
    verifica se uma linha (completa ou parcial) é consistente com a pista.
    """
    if clue == 0:
        return True

    if complete:
        return count_visible(line) == clue

    prefix = []
    for h in line:
        if h == 0:
            break
        prefix.append(h)

    return count_visible(prefix) <= clue


def board_consistent(board: Board) -> bool:
    """
    verifica todas as restriçoes do tabuleiro (parcial ou completo)
    """
    n = board.get_order()
    clues = board.get_clues()
    grid = board.get_inner_grid()

    for c in range(n):
        col = [grid[r][c] for r in range(n)]
        complete = all(x != 0 for x in col)

        if not is_line_valid(col, n):
            return False
        if not is_consistent(col, clues['top'][c], complete):
            return False
        if not is_consistent(list(reversed(col)), clues['bottom'][c], complete):
            return False

    for r in range(n):
        row = grid[r]
        complete = all(x != 0 for x in row)

        if not is_line_valid(row, n):
            return False
        if not is_consistent(row, clues['left'][r], complete):
            return False
        if not is_consistent(list(reversed(row)), clues['right'][r], complete):
            return False

    return True

def solve(board: Board) -> bool:
    """
    retorna true se encontrou solução e false se não
    """
    n = board.get_order()

    # proxima célula vazia
    next_cell = None
    for r in range(1, n + 1):
        for c in range(1, n + 1):
            if board.get_cell(r, c) == 0:
                next_cell = (r, c)
                break
        if next_cell:
            break

    if next_cell is None:
        return board_consistent(board)

    r, c = next_cell

    for val in range(1, n + 1):
        board.set_cell(r, c, val)

        if board_consistent(board):
            if solve(board):
                return True

        board.set_cell(r, c, 0)

    return False