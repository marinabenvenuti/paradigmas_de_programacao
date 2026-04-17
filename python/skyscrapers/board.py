import json


class Board:
    def __init__(self, id, order):
        self.__order = order
        self.__matrix = [[0] * (self.__order + 2) for _ in range(self.__order + 2)]

        with open('puzzles.json', 'r', encoding='utf-8') as file:
            puzzle = json.load(file)

        for i in range(4):
            self.__matrix[0][i+1] = puzzle['puzzles'][id]['line0'][i]
            self.__matrix[i+1][self.__order+1] = puzzle['puzzles'][id]['line1'][i]
            self.__matrix[self.__order+1][i+1] = puzzle['puzzles'][id]['line2'][i]
            self.__matrix[i+1][0] = puzzle['puzzles'][id]['line3'][i]

    def print_board(self):
        for i in range(self.__order+2):
            print(f"{self.__matrix[i]}\n")
