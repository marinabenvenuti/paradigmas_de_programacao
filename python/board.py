class Board:
    def __init__(self, puzzle: dict, order: int):
        self.__order = order
        self.__matrix = [[0] * (self.__order + 2) for _ in range(self.__order + 2)]
        for i in range(self.__order):
            self.__matrix[0][i + 1]              = puzzle['line0'][i]
            self.__matrix[i + 1][self.__order + 1] = puzzle['line1'][i]
            self.__matrix[self.__order + 1][i + 1] = puzzle['line2'][i]
            self.__matrix[i + 1][0]              = puzzle['line3'][i]

    def get_order(self) -> int:
        return self.__order

    def get_clues(self) -> dict:
        """retorna as pistas nas 4 bordas como listas separadas"""
        o = self.__order
        return {
            'top':    [self.__matrix[0][c]     for c in range(1, o + 1)],
            'right':  [self.__matrix[r][o + 1] for r in range(1, o + 1)],
            'bottom': [self.__matrix[o + 1][c] for c in range(1, o + 1)],
            'left':   [self.__matrix[r][0]     for r in range(1, o + 1)],
        }

    def set_cell(self, row: int, col: int, value: int):
        """define o valor de uma célula interior"""
        self.__matrix[row][col] = value

    def get_cell(self, row: int, col: int) -> int:
        return self.__matrix[row][col]

    def get_inner_grid(self) -> list[list[int]]:
        """retorna apenas a grade interior (sem as bordas)"""
        o = self.__order
        return [
            [self.__matrix[r][c] for c in range(1, o + 1)]
            for r in range(1, o + 1)
        ]

    def print_board(self):
        o = self.__order
        clues = self.get_clues()

        # topo
        print("    " + "  ".join(str(x) for x in clues['top']))
        print("   +" + "---" * o + "+")

        # linhas internas
        for r in range(1, o + 1):
            left  = clues['left'][r - 1]
            right = clues['right'][r - 1]
            row   = " ".join(
                str(self.__matrix[r][c]) if self.__matrix[r][c] != 0 else "·"
                for c in range(1, o + 1)
            )
            print(f" {left} | {row} | {right}")

        # base
        print("   +" + "---" * o + "+")
        print("    " + "  ".join(str(x) for x in clues['bottom']))