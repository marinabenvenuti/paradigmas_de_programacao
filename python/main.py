import json
from board import Board
from solver import solve


def load_puzzle(puzzle_id: int, filepath: str = 'puzzles.json') -> tuple[dict, int]:
    with open(filepath, 'r', encoding='utf-8') as f:
        data = json.load(f)
    puzzle = data['puzzles'][puzzle_id]
    order = len(puzzle['line0'])
    return puzzle, order


def main():
    puzzle_id = 1
    puzzle, order = load_puzzle(puzzle_id)

    board = Board(puzzle, order)

    print(f"Puzzle #{puzzle_id} (grade {order}x{order})")
    board.print_board()

    print("Resolvendo...")
    solved = solve(board)

    if solved:
        print("=== Solução ===")
        board.print_board()
    else:
        print("Sem solução.")


if __name__ == '__main__':
    main()