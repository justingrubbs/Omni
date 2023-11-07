import random
import time
import pygame
from pygame.locals import * # this lets you have more control

# Still need to add click and remove the printed prompts
# Still need to add penciling


def start() -> int:
    valid = False
    num = 0
    play_or_solve = input('Would you like to play Sudoku or would you like me to solve one?\nPlease enter "play" or "solve":\n').lower()
    while not valid:
        if play_or_solve == '' or ' ' in play_or_solve:
            play_or_solve = input('That is not a valid input.\nPlease enter either "play" or "solve":\n').lower()
            valid = False
        elif play_or_solve == 'play':
            num = 1
            valid = True
        elif play_or_solve == 'solve':
            num = 2
            valid = True
        else:
            play_or_solve = input(f'{play_or_solve} is not a valid input.\nPlease enter either "play" or "solve":\n').lower()
            valid = False
    return num


def valid_box(boxes: tuple) -> int:
    box_parameter = 0
    for n in boxes:
        one = 0
        two = 0
        three = 0
        four = 0
        five = 0
        six = 0
        seven = 0
        eight = 0
        nine = 0
        zero = 0
        for i in n:
            if i == 1:
                one += 1
            elif i == 2:
                two += 1
            elif i == 3:
                three += 1
            elif i == 4:
                four += 1
            elif i == 5:
                five += 1
            elif i == 6:
                six += 1
            elif i == 7:
                seven += 1
            elif i == 8:
                eight += 1
            elif i == 9:
                nine += 1
            else:
                zero += 1
        count_list = [one, two, three, four, five, six, seven, eight, nine]
        for value in count_list:
            if value >= 2:
                box_parameter = 0
                return box_parameter
            elif count_list == [1, 1, 1, 1, 1, 1, 1, 1, 1]:
                box_parameter = 2
            else:
                box_parameter = 1
    return box_parameter


def valid_column(columns: tuple, boxes: tuple) -> int:
    column_parameter = 0
    for n in columns:
        one = 0
        two = 0
        three = 0
        four = 0
        five = 0
        six = 0
        seven = 0
        eight = 0
        nine = 0
        zero = 0
        length = 9 - len(n)
        for i in n:
            if i == 1:
                one += 1
            elif i == 2:
                two += 1
            elif i == 3:
                three += 1
            elif i == 4:
                four += 1
            elif i == 5:
                five += 1
            elif i == 6:
                six += 1
            elif i == 7:
                seven += 1
            elif i == 8:
                eight += 1
            elif i == 9:
                nine += 1
            else:
                zero += 1
                if zero > length:
                    column_parameter = 0
                    return column_parameter
        count_list = [one, two, three, four, five, six, seven, eight, nine]
        for value in count_list:
            if value >= 2:
                column_parameter = 0
                return column_parameter
            elif count_list == [1, 1, 1, 1, 1, 1, 1, 1, 1]:
                column_parameter = valid_box(boxes)
            else:
                column_parameter = valid_box(boxes)
            if column_parameter == 0:
                return column_parameter
    return column_parameter


def count_timeout(count: int, master_count: int, line: int):
    print('Timeout error :(\nPlease restart:\n')
    print(f'Count: {count}')
    print(f'Master count: {master_count}')
    print(f'Line: {line}')
    column_parameter = 2
    return


# Along with "valid_box" and "valid_column", this generates the boxes, rows, and columns in the grid with the master key
def grid_generator(column_parameter: int) -> tuple:
    row_0 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    random.shuffle(row_0)
    while column_parameter < 2:
        count = 0
        master_count = 0
        while column_parameter == 0:
            row_1 = random.sample(row_0, 9)
            column_0 = [row_0[0], row_1[0]]
            column_1 = [row_0[1], row_1[1]]
            column_2 = [row_0[2], row_1[2]]
            column_3 = [row_0[3], row_1[3]]
            column_4 = [row_0[4], row_1[4]]
            column_5 = [row_0[5], row_1[5]]
            column_6 = [row_0[6], row_1[6]]
            column_7 = [row_0[7], row_1[7]]
            column_8 = [row_0[8], row_1[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8]]
            boxes = (box_0, box_1, box_2)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count, 165)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_2 = random.sample(row_1, 9)
            column_0 = [row_0[0], row_1[0], row_2[0]]
            column_1 = [row_0[1], row_1[1], row_2[1]]
            column_2 = [row_0[2], row_1[2], row_2[2]]
            column_3 = [row_0[3], row_1[3], row_2[3]]
            column_4 = [row_0[4], row_1[4], row_2[4]]
            column_5 = [row_0[5], row_1[5], row_2[5]]
            column_6 = [row_0[6], row_1[6], row_2[6]]
            column_7 = [row_0[7], row_1[7], row_2[7]]
            column_8 = [row_0[8], row_1[8], row_2[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            boxes = (box_0, box_1, box_2)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count, 190)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_3 = random.sample(row_2, 9)
            column_0 = [row_0[0], row_1[0], row_2[0], row_3[0]]
            column_1 = [row_0[1], row_1[1], row_2[1], row_3[1]]
            column_2 = [row_0[2], row_1[2], row_2[2], row_3[2]]
            column_3 = [row_0[3], row_1[3], row_2[3], row_3[3]]
            column_4 = [row_0[4], row_1[4], row_2[4], row_3[4]]
            column_5 = [row_0[5], row_1[5], row_2[5], row_3[5]]
            column_6 = [row_0[6], row_1[6], row_2[6], row_3[6]]
            column_7 = [row_0[7], row_1[7], row_2[7], row_3[7]]
            column_8 = [row_0[8], row_1[8], row_2[8], row_3[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            box_3 = [row_3[0], row_3[1], row_3[2]]
            box_4 = [row_3[3], row_3[4], row_3[5]]
            box_5 = [row_3[6], row_3[7], row_3[8]]
            boxes = (box_0, box_1, box_2, box_3, box_4, box_5)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count, 218)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_4 = random.sample(row_3, 9)
            column_0 = [row_0[0], row_1[0], row_2[0], row_3[0], row_4[0]]
            column_1 = [row_0[1], row_1[1], row_2[1], row_3[1], row_4[1]]
            column_2 = [row_0[2], row_1[2], row_2[2], row_3[2], row_4[2]]
            column_3 = [row_0[3], row_1[3], row_2[3], row_3[3], row_4[3]]
            column_4 = [row_0[4], row_1[4], row_2[4], row_3[4], row_4[4]]
            column_5 = [row_0[5], row_1[5], row_2[5], row_3[5], row_4[5]]
            column_6 = [row_0[6], row_1[6], row_2[6], row_3[6], row_4[6]]
            column_7 = [row_0[7], row_1[7], row_2[7], row_3[7], row_4[7]]
            column_8 = [row_0[8], row_1[8], row_2[8], row_3[8], row_4[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            box_3 = [row_3[0], row_3[1], row_3[2], row_4[0], row_4[1], row_4[2]]
            box_4 = [row_3[3], row_3[4], row_3[5], row_4[3], row_4[4], row_4[5]]
            box_5 = [row_3[6], row_3[7], row_3[8], row_4[6], row_4[7], row_4[8]]
            boxes = (box_0, box_1, box_2, box_3, box_4, box_5)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count,246)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_5 = random.sample(row_4, 9)
            column_0 = [row_0[0], row_1[0], row_2[0], row_3[0], row_4[0], row_5[0]]
            column_1 = [row_0[1], row_1[1], row_2[1], row_3[1], row_4[1], row_5[1]]
            column_2 = [row_0[2], row_1[2], row_2[2], row_3[2], row_4[2], row_5[2]]
            column_3 = [row_0[3], row_1[3], row_2[3], row_3[3], row_4[3], row_5[3]]
            column_4 = [row_0[4], row_1[4], row_2[4], row_3[4], row_4[4], row_5[4]]
            column_5 = [row_0[5], row_1[5], row_2[5], row_3[5], row_4[5], row_5[5]]
            column_6 = [row_0[6], row_1[6], row_2[6], row_3[6], row_4[6], row_5[6]]
            column_7 = [row_0[7], row_1[7], row_2[7], row_3[7], row_4[7], row_5[7]]
            column_8 = [row_0[8], row_1[8], row_2[8], row_3[8], row_4[8], row_5[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            box_3 = [row_3[0], row_3[1], row_3[2], row_4[0], row_4[1], row_4[2], row_5[0], row_5[1], row_5[2]]
            box_4 = [row_3[3], row_3[4], row_3[5], row_4[3], row_4[4], row_4[5], row_5[3], row_5[4], row_5[5]]
            box_5 = [row_3[6], row_3[7], row_3[8], row_4[6], row_4[7], row_4[8], row_5[6], row_5[7], row_5[8]]
            boxes = (box_0, box_1, box_2, box_3, box_4, box_5)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count,274)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_6 = random.sample(row_5, 9)
            column_0 = [row_0[0], row_1[0], row_2[0], row_3[0], row_4[0], row_5[0], row_6[0]]
            column_1 = [row_0[1], row_1[1], row_2[1], row_3[1], row_4[1], row_5[1], row_6[1]]
            column_2 = [row_0[2], row_1[2], row_2[2], row_3[2], row_4[2], row_5[2], row_6[2]]
            column_3 = [row_0[3], row_1[3], row_2[3], row_3[3], row_4[3], row_5[3], row_6[3]]
            column_4 = [row_0[4], row_1[4], row_2[4], row_3[4], row_4[4], row_5[4], row_6[4]]
            column_5 = [row_0[5], row_1[5], row_2[5], row_3[5], row_4[5], row_5[5], row_6[5]]
            column_6 = [row_0[6], row_1[6], row_2[6], row_3[6], row_4[6], row_5[6], row_6[6]]
            column_7 = [row_0[7], row_1[7], row_2[7], row_3[7], row_4[7], row_5[7], row_6[7]]
            column_8 = [row_0[8], row_1[8], row_2[8], row_3[8], row_4[8], row_5[8], row_6[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            box_3 = [row_3[0], row_3[1], row_3[2], row_4[0], row_4[1], row_4[2], row_5[0], row_5[1], row_5[2]]
            box_4 = [row_3[3], row_3[4], row_3[5], row_4[3], row_4[4], row_4[5], row_5[3], row_5[4], row_5[5]]
            box_5 = [row_3[6], row_3[7], row_3[8], row_4[6], row_4[7], row_4[8], row_5[6], row_5[7], row_5[8]]
            box_6 = [row_6[0], row_6[1], row_6[2]]
            box_7 = [row_6[3], row_6[4], row_6[5]]
            box_8 = [row_6[6], row_6[7], row_6[8]]
            boxes = (box_0, box_1, box_2, box_3, box_4, box_5, box_6, box_7, box_8)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count, 305)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_7 = random.sample(row_6, 9)
            column_0 = [row_0[0], row_1[0], row_2[0], row_3[0], row_4[0], row_5[0], row_6[0], row_7[0]]
            column_1 = [row_0[1], row_1[1], row_2[1], row_3[1], row_4[1], row_5[1], row_6[1], row_7[1]]
            column_2 = [row_0[2], row_1[2], row_2[2], row_3[2], row_4[2], row_5[2], row_6[2], row_7[2]]
            column_3 = [row_0[3], row_1[3], row_2[3], row_3[3], row_4[3], row_5[3], row_6[3], row_7[3]]
            column_4 = [row_0[4], row_1[4], row_2[4], row_3[4], row_4[4], row_5[4], row_6[4], row_7[4]]
            column_5 = [row_0[5], row_1[5], row_2[5], row_3[5], row_4[5], row_5[5], row_6[5], row_7[5]]
            column_6 = [row_0[6], row_1[6], row_2[6], row_3[6], row_4[6], row_5[6], row_6[6], row_7[6]]
            column_7 = [row_0[7], row_1[7], row_2[7], row_3[7], row_4[7], row_5[7], row_6[7], row_7[7]]
            column_8 = [row_0[8], row_1[8], row_2[8], row_3[8], row_4[8], row_5[8], row_6[8], row_7[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            box_3 = [row_3[0], row_3[1], row_3[2], row_4[0], row_4[1], row_4[2], row_5[0], row_5[1], row_5[2]]
            box_4 = [row_3[3], row_3[4], row_3[5], row_4[3], row_4[4], row_4[5], row_5[3], row_5[4], row_5[5]]
            box_5 = [row_3[6], row_3[7], row_3[8], row_4[6], row_4[7], row_4[8], row_5[6], row_5[7], row_5[8]]
            box_6 = [row_6[0], row_6[1], row_6[2], row_7[0], row_7[1], row_7[2]]
            box_7 = [row_6[3], row_6[4], row_6[5], row_7[3], row_7[4], row_7[5]]
            box_8 = [row_6[6], row_6[7], row_6[8], row_7[6], row_7[7], row_7[8]]
            boxes = (box_0, box_1, box_2, box_3, box_4, box_5, box_6, box_7, box_8)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count, 336)
        master_count += count
        count = 0
        column_parameter = 0
        while column_parameter == 0:
            row_8 = random.sample(row_7, 9)
            column_0 = [row_0[0], row_1[0], row_2[0], row_3[0], row_4[0], row_5[0], row_6[0], row_7[0], row_8[0]]
            column_1 = [row_0[1], row_1[1], row_2[1], row_3[1], row_4[1], row_5[1], row_6[1], row_7[1], row_8[1]]
            column_2 = [row_0[2], row_1[2], row_2[2], row_3[2], row_4[2], row_5[2], row_6[2], row_7[2], row_8[2]]
            column_3 = [row_0[3], row_1[3], row_2[3], row_3[3], row_4[3], row_5[3], row_6[3], row_7[3], row_8[3]]
            column_4 = [row_0[4], row_1[4], row_2[4], row_3[4], row_4[4], row_5[4], row_6[4], row_7[4], row_8[4]]
            column_5 = [row_0[5], row_1[5], row_2[5], row_3[5], row_4[5], row_5[5], row_6[5], row_7[5], row_8[5]]
            column_6 = [row_0[6], row_1[6], row_2[6], row_3[6], row_4[6], row_5[6], row_6[6], row_7[6], row_8[6]]
            column_7 = [row_0[7], row_1[7], row_2[7], row_3[7], row_4[7], row_5[7], row_6[7], row_7[7], row_8[7]]
            column_8 = [row_0[8], row_1[8], row_2[8], row_3[8], row_4[8], row_5[8], row_6[8], row_7[8], row_8[8]]
            columns = (column_0, column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8)

            box_0 = [row_0[0], row_0[1], row_0[2], row_1[0], row_1[1], row_1[2], row_2[0], row_2[1], row_2[2]]
            box_1 = [row_0[3], row_0[4], row_0[5], row_1[3], row_1[4], row_1[5], row_2[3], row_2[4], row_2[5]]
            box_2 = [row_0[6], row_0[7], row_0[8], row_1[6], row_1[7], row_1[8], row_2[6], row_2[7], row_2[8]]
            box_3 = [row_3[0], row_3[1], row_3[2], row_4[0], row_4[1], row_4[2], row_5[0], row_5[1], row_5[2]]
            box_4 = [row_3[3], row_3[4], row_3[5], row_4[3], row_4[4], row_4[5], row_5[3], row_5[4], row_5[5]]
            box_5 = [row_3[6], row_3[7], row_3[8], row_4[6], row_4[7], row_4[8], row_5[6], row_5[7], row_5[8]]
            box_6 = [row_6[0], row_6[1], row_6[2], row_7[0], row_7[1], row_7[2], row_8[0], row_8[1], row_8[2]]
            box_7 = [row_6[3], row_6[4], row_6[5], row_7[3], row_7[4], row_7[5], row_8[3], row_8[4], row_8[5]]
            box_8 = [row_6[6], row_6[7], row_6[8], row_7[6], row_7[7], row_7[8], row_8[6], row_8[7], row_8[8]]
            boxes = (box_0, box_1, box_2, box_3, box_4, box_5, box_6, box_7, box_8)
            column_parameter = valid_column(columns, boxes)
            count += 1
            if count == 1000000:
                master_count += count
                count_timeout(count, master_count, 367)
        column_parameter = 2
    rows = (row_0, row_1, row_2, row_3, row_4, row_5, row_6, row_7, row_8)
    return (rows, columns, boxes)


# Returns "current grid" rather than directly changing the values of "grid"
# Returns a tuple containing lists ranging from 0 to 8, representing the grid with 0s instead of blanks
def current_grid_generator(grid: tuple) -> tuple:
    current_grid_0 = []
    current_grid_1 = []
    current_grid_2 = []
    current_grid_3 = []
    current_grid_4 = []
    current_grid_5 = []
    current_grid_6 = []
    current_grid_7 = []
    current_grid_8 = []
    current_grid = (current_grid_0, current_grid_1, current_grid_2, current_grid_3, current_grid_4, current_grid_5, current_grid_6, current_grid_7, current_grid_8)
    i = 0
    while i <= 8:
        for n in grid[i]:
            current_grid[i].append(n)
        i += 1
    i = 56
    num_list = []
    while i > 0:
        n = random.randint(0, 80)
        if n not in num_list:
            i -= 1
            num_list.append(n)
            if (n >= 0) and (n <= 8):
                current_grid[0][n] = 0
            elif (n >= 9) and (n <= 17):
                current_grid[1][n - 9] = 0
            elif (n >= 18) and (n <= 26):
                current_grid[2][n - 18] = 0
            elif (n >= 27) and (n <= 35):
                current_grid[3][n - 27] = 0
            elif (n >= 36) and (n <= 44):
                current_grid[4][n - 36] = 0
            elif (n >= 45) and (n <= 53):
                current_grid[5][n - 45] = 0
            elif (n >= 54) and (n <= 62):
                current_grid[6][n - 54] = 0
            elif (n >= 63) and (n <= 71):
                current_grid[7][n - 63] = 0
            elif (n >= 72) and (n <= 80):
                current_grid[8][n - 72] = 0
            else:
                print('Error has occurred in the random number selection and removal process: ')
    return current_grid


# Generates a dictionary that functions as the answer key
def master_dictionary(rows: tuple) -> dict:
    master_dict = {}
    n = 1
    index_of_index = 0
    index = 0
    while n <= 81:
        if index_of_index == 9:
            index += 1
            index_of_index = 0
        master_dict[n] = rows[index][index_of_index]
        index_of_index += 1
        n += 1
    return master_dict


# Generates a dictionary representing the Sudoku "board" with 0s to indicate a blank space
# Game ends when this is equivalent to the dictionary generated in game_dict_generator
def current_dictionary(current_grid: tuple) -> dict:
    row_0 = current_grid[0]
    row_1 = current_grid[1]
    row_2 = current_grid[2]
    row_3 = current_grid[3]
    row_4 = current_grid[4]
    row_5 = current_grid[5]
    row_6 = current_grid[6]
    row_7 = current_grid[7]
    row_8 = current_grid[8]
    rows = (row_0, row_1, row_2, row_3, row_4, row_5, row_6, row_7, row_8)
    current_dict = {1: rows[0][0], 2: rows[0][1], 3: rows[0][2], 4: rows[0][3], 5: rows[0][4], 6: rows[0][5], 7: rows[0][6], 8: rows[0][7], 9: rows[0][8], 10: rows[1][0], 11: rows[1][1], 12: rows[1][2], 13: rows[1][3], 14: rows[1][4], 15: rows[1][5], 16: rows[1][6], 17: rows[1][7], 18: rows[1][8], 19: rows[2][0], 20: rows[2][1], 21: rows[2][2], 22: rows[2][3], 23: rows[2][4], 24: rows[2][5], 25: rows[2][6], 26: rows[2][7], 27: rows[2][8], 28: rows[3][0], 29: rows[3][1], 30: rows[3][2], 31: rows[3][3], 32: rows[3][4], 33: rows[3][5], 34: rows[3][6], 35: rows[3][7], 36: rows[3][8], 37: rows[4][0], 38: rows[4][1], 39: rows[4][2], 40: rows[4][3], 41: rows[4][4], 42: rows[4][5], 43: rows[4][6], 44: rows[4][7], 45: rows[4][8], 46: rows[5][0], 47: rows[5][1], 48: rows[5][2], 49: rows[5][3], 50: rows[5][4], 51: rows[5][5], 52: rows[5][6], 53: rows[5][7], 54: rows[5][8], 55: rows[6][0], 56: rows[6][1], 57: rows[6][2], 58: rows[6][3], 59: rows[6][4], 60: rows[6][5], 61: rows[6][6], 62: rows[6][7], 63: rows[6][8], 64: rows[7][0], 65: rows[7][1], 66: rows[7][2], 67: rows[7][3], 68: rows[7][4], 69: rows[7][5], 70: rows[7][6], 71: rows[7][7], 72: rows[7][8], 73: rows[8][0], 74: rows[8][1], 75: rows[8][2], 76: rows[8][3], 77: rows[8][4], 78: rows[8][5], 79: rows[8][6], 80: rows[8][7], 81: rows[8][8]}
    return current_dict


def index_dictionary(index_groups: tuple) -> dict:
    index_dict = {}
    row_indexes = index_groups[0]
    column_indexes = index_groups[1]
    box_indexes = index_groups[2]
    i = 1
    while i <= 81:
        x = []
        y = []
        z = []
        n = 0
        while x != row_indexes[n]:
            if i in row_indexes[n]:
                x = row_indexes[n]
            else:
                n += 1
        n = 0
        while y != column_indexes[n]:
            if i in column_indexes[n]:
                y = column_indexes[n]
            else:
                n += 1
        n = 0
        while z != box_indexes[n]:
            if i in box_indexes[n]:
                z = box_indexes[n]
            else:
                n += 1
        index_dict[i] = (x, y, z)
        i += 1
    return index_dict


def is_valid_puzzle(groups: tuple) -> bool:
    valid_puzzle = True
    rows = groups[0]
    columns = groups[1]
    boxes = groups[2]
    i = 0
    while i <= 8:
        sum = 0
        for n in rows[i]:
            sum += n
        if sum != 45:
            print(f'There has been an error in rows{[i]} in "is_valid_puzzle"')
            valid_puzzle = False
            return valid_puzzle
        i += 1
    i = 0
    while i <= 8:
        sum = 0
        for n in columns[i]:
            sum += n
        if sum != 45:
            print(f'There has been an error in columns{[i]} in "is_valid_puzzle"')
            valid_puzzle = False
            return valid_puzzle
        i += 1
    i = 0
    while i <= 8:
        sum = 0
        for n in boxes[i]:
            sum += n
        if sum != 45:
            print(f'There has been an error in boxes{[i]} in "is_valid_puzzle"')
            print(sum)
            valid_puzzle = False
            return valid_puzzle
        i += 1
    return valid_puzzle


    # Ensure every digit appears in every row, column, and box once and only once


# Generates every row, column, and box in the grid -- answer key
def current_group_generator(current_grid: tuple) -> tuple:
    current_row_0 = current_grid[0]
    current_row_1 = current_grid[1]
    current_row_2 = current_grid[2]
    current_row_3 = current_grid[3]
    current_row_4 = current_grid[4]
    current_row_5 = current_grid[5]
    current_row_6 = current_grid[6]
    current_row_7 = current_grid[7]
    current_row_8 = current_grid[8]
    current_rows = (current_row_0, current_row_1, current_row_2, current_row_3, current_row_4, current_row_5, current_row_6, current_row_7, current_row_8)

    current_column_0 = [current_rows[0][0], current_rows[1][0], current_rows[2][0], current_rows[3][0], current_rows[4][0], current_rows[5][0], current_rows[6][0], current_rows[7][0], current_rows[8][0]]
    current_column_1 = [current_rows[0][1], current_rows[1][1], current_rows[2][1], current_rows[3][1], current_rows[4][1], current_rows[5][1], current_rows[6][1], current_rows[7][1], current_rows[8][1]]
    current_column_2 = [current_rows[0][2], current_rows[1][2], current_rows[2][2], current_rows[3][2], current_rows[4][2], current_rows[5][2], current_rows[6][2], current_rows[7][2], current_rows[8][2]]
    current_column_3 = [current_rows[0][3], current_rows[1][3], current_rows[2][3], current_rows[3][3], current_rows[4][3], current_rows[5][3], current_rows[6][3], current_rows[7][3], current_rows[8][3]]
    current_column_4 = [current_rows[0][4], current_rows[1][4], current_rows[2][4], current_rows[3][4], current_rows[4][4], current_rows[5][4], current_rows[6][4], current_rows[7][4], current_rows[8][4]]
    current_column_5 = [current_rows[0][5], current_rows[1][5], current_rows[2][5], current_rows[3][5], current_rows[4][5], current_rows[5][5], current_rows[6][5], current_rows[7][5], current_rows[8][5]]
    current_column_6 = [current_rows[0][6], current_rows[1][6], current_rows[2][6], current_rows[3][6], current_rows[4][6], current_rows[5][6], current_rows[6][6], current_rows[7][6], current_rows[8][6]]
    current_column_7 = [current_rows[0][7], current_rows[1][7], current_rows[2][7], current_rows[3][7], current_rows[4][7], current_rows[5][7], current_rows[6][7], current_rows[7][7], current_rows[8][7]]
    current_column_8 = [current_rows[0][8], current_rows[1][8], current_rows[2][8], current_rows[3][8], current_rows[4][8], current_rows[5][8], current_rows[6][8], current_rows[7][8], current_rows[8][8]]
    current_columns = (current_column_0, current_column_1, current_column_2, current_column_3, current_column_4, current_column_5, current_column_6, current_column_7, current_column_8)

    current_box_0 = [current_rows[0][0], current_rows[0][1], current_rows[0][2], current_rows[1][0], current_rows[1][1], current_rows[1][2], current_rows[2][0], current_rows[2][1], current_rows[2][2]]
    current_box_1 = [current_rows[0][3], current_rows[0][4], current_rows[0][5], current_rows[1][3], current_rows[1][4], current_rows[1][5], current_rows[2][3], current_rows[2][4], current_rows[2][5]]
    current_box_2 = [current_rows[0][6], current_rows[0][7], current_rows[0][8], current_rows[1][6], current_rows[1][7], current_rows[1][8], current_rows[2][6], current_rows[2][7], current_rows[2][8]]
    current_box_3 = [current_rows[3][0], current_rows[3][1], current_rows[3][2], current_rows[4][0], current_rows[4][1], current_rows[4][2], current_rows[5][0], current_rows[5][1], current_rows[5][2]]
    current_box_4 = [current_rows[3][3], current_rows[3][4], current_rows[3][5], current_rows[4][3], current_rows[4][4], current_rows[4][5], current_rows[5][3], current_rows[5][4], current_rows[5][5]]
    current_box_5 = [current_rows[3][6], current_rows[3][7], current_rows[3][8], current_rows[4][6], current_rows[4][7], current_rows[4][8], current_rows[5][6], current_rows[5][7], current_rows[5][8]]
    current_box_6 = [current_rows[6][0], current_rows[6][1], current_rows[6][2], current_rows[7][0], current_rows[7][1], current_rows[7][2], current_rows[8][0], current_rows[8][1], current_rows[8][2]]
    current_box_7 = [current_rows[6][3], current_rows[6][4], current_rows[6][5], current_rows[7][3], current_rows[7][4], current_rows[7][5], current_rows[8][3], current_rows[8][4], current_rows[8][5]]
    current_box_8 = [current_rows[6][6], current_rows[6][7], current_rows[6][8], current_rows[7][6], current_rows[7][7], current_rows[7][8], current_rows[8][6], current_rows[8][7], current_rows[8][8]]
    current_boxes = (current_box_0, current_box_1, current_box_2, current_box_3, current_box_4, current_box_5, current_box_6, current_box_7, current_box_8)
    return (current_rows, current_columns, current_boxes)


# Directly relates to the rows, columns, and box generated in sudoku_grid_organization
# Contains the index of every individual square rather than the answer-key or its current value
def index_group_generator() -> tuple:
    row_index_0 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    row_index_1 = [10, 11, 12, 13, 14, 15, 16, 17, 18]
    row_index_2 = [19, 20, 21, 22, 23, 24, 25, 26, 27]
    row_index_3 = [28, 29, 30, 31, 32, 33, 34, 35, 36]
    row_index_4 = [37, 38, 39, 40, 41, 42, 43, 44, 45]
    row_index_5 = [46, 47, 48, 49, 50, 51, 52, 53, 54]
    row_index_6 = [55, 56, 57, 58, 59, 60, 61, 62, 63]
    row_index_7 = [64, 65, 66, 67, 68, 69, 70, 71, 72]
    row_index_8 = [73, 74, 75, 76, 77, 78, 79, 80, 81]
    row_indexes = (row_index_0, row_index_1, row_index_2, row_index_3, row_index_4, row_index_5, row_index_6, row_index_7, row_index_8)

    column_index_0 = [1, 10, 19, 28, 37, 46, 55, 64, 73]
    column_index_1 = [2, 11, 20, 29, 38, 47, 56, 65, 74]
    column_index_2 = [3, 12, 21, 30, 39, 48, 57, 66, 75]
    column_index_3 = [4, 13, 22, 31, 40, 49, 58, 67, 76]
    column_index_4 = [5, 14, 23, 32, 41, 50, 59, 68, 77]
    column_index_5 = [6, 15, 24, 33, 42, 51, 60, 69, 78]
    column_index_6 = [7, 16, 25, 34, 43, 52, 61, 70, 79]
    column_index_7 = [8, 17, 26, 35, 44, 53, 62, 71, 80]
    column_index_8 = [9, 18, 27, 36, 45, 54, 63, 72, 81]
    column_indexes = (column_index_0, column_index_1, column_index_2, column_index_3, column_index_4, column_index_5, column_index_6, column_index_7, column_index_8)

    box_index_0 = [1, 2, 3, 10, 11, 12, 19, 20, 21]
    box_index_1 = [4, 5, 6, 13, 14, 15, 22, 23, 24]
    box_index_2 = [7, 8, 9, 16, 17, 18, 25, 26, 27]
    box_index_3 = [28, 29, 30, 37, 38, 39, 46, 47, 48]
    box_index_4 = [31, 32, 33, 40, 41, 42, 49, 50, 51]
    box_index_5 = [34, 35, 36, 43, 44, 45, 52, 53, 54]
    box_index_6 = [55, 56, 57, 64, 65, 66, 73, 74, 75]
    box_index_7 = [58, 59, 60, 67, 68, 69, 76, 77, 78]
    box_index_8 = [61, 62, 63, 70, 71, 72, 79, 80, 81]
    box_indexes = (box_index_0, box_index_1, box_index_2, box_index_3, box_index_4, box_index_5, box_index_6, box_index_7, box_index_8)
    return (row_indexes, column_indexes, box_indexes)


def print_color_grid(current_dict: dict, selected_square: int, valid_selection: bool, selected_value: int, correct_selection: bool):
    temporary_dict = {}
    for n in current_dict:
        k = current_dict[n]
        if k == 0:
            k = '   '
            temporary_dict[n] = k
        elif k != 0:
            temporary_dict[n] = f' {k} '
    if selected_square != 0:
        if valid_selection:
            if selected_value > 0:
                if correct_selection:
                    temporary_dict[selected_square] = (f'\033[1;30;42m {selected_value} \033[0m')
                elif not correct_selection:
                    temporary_dict[selected_square] = (f'\033[1;30;41m {selected_value} \033[0m')
            else:
                temporary_dict[selected_square] = (f'\033[1;34;44m{temporary_dict[selected_square]}\033[0m')
        elif not valid_selection:
            temporary_dict[selected_square] = (f'\033[1;30;43m{temporary_dict[selected_square]}\033[0m')
    picture_grid = (
        f'\n+-----------+-----------+-----------+\n|{temporary_dict[1]} {temporary_dict[2]} {temporary_dict[3]}|{temporary_dict[4]} {temporary_dict[5]} {temporary_dict[6]}|{temporary_dict[7]} {temporary_dict[8]} {temporary_dict[9]}|\n|{temporary_dict[10]} {temporary_dict[11]} {temporary_dict[12]}|{temporary_dict[13]} {temporary_dict[14]} {temporary_dict[15]}|{temporary_dict[16]} {temporary_dict[17]} {temporary_dict[18]}|\n|{temporary_dict[19]} {temporary_dict[20]} {temporary_dict[21]}|{temporary_dict[22]} {temporary_dict[23]} {temporary_dict[24]}|{temporary_dict[25]} {temporary_dict[26]} {temporary_dict[27]}|\n+-----------+-----------+-----------+\n|{temporary_dict[28]} {temporary_dict[29]} {temporary_dict[30]}|{temporary_dict[31]} {temporary_dict[32]} {temporary_dict[33]}|{temporary_dict[34]} {temporary_dict[35]} {temporary_dict[36]}|\n|{temporary_dict[37]} {temporary_dict[38]} {temporary_dict[39]}|{temporary_dict[40]} {temporary_dict[41]} {temporary_dict[42]}|{temporary_dict[43]} {temporary_dict[44]} {temporary_dict[45]}|\n|{temporary_dict[46]} {temporary_dict[47]} {temporary_dict[48]}|{temporary_dict[49]} {temporary_dict[50]} {temporary_dict[51]}|{temporary_dict[52]} {temporary_dict[53]} {temporary_dict[54]}|\n+-----------+-----------+-----------+\n|{temporary_dict[55]} {temporary_dict[56]} {temporary_dict[57]}|{temporary_dict[58]} {temporary_dict[59]} {temporary_dict[60]}|{temporary_dict[61]} {temporary_dict[62]} {temporary_dict[63]}|\n|{temporary_dict[64]} {temporary_dict[65]} {temporary_dict[66]}|{temporary_dict[67]} {temporary_dict[68]} {temporary_dict[69]}|{temporary_dict[70]} {temporary_dict[71]} {temporary_dict[72]}|\n|{temporary_dict[73]} {temporary_dict[74]} {temporary_dict[75]}|{temporary_dict[76]} {temporary_dict[77]} {temporary_dict[78]}|{temporary_dict[79]} {temporary_dict[80]} {temporary_dict[81]}|\n+-----------+-----------+-----------+')
    print(picture_grid)
    return


def square_selection() -> int:
    valid = False
    selected_square = input('Select a square by typing an integer from 1 to 81:\n')
    while not valid:
        if selected_square == '' or ' ' in selected_square:
            selected_square = input('That is not a valid input. Please enter an integer from 1 to 81:\n')
        elif selected_square < '1' or selected_square > '81' or len(selected_square) > 2:
            selected_square = input(f'{selected_square} is not a valid input. Please enter an integer from 1 to 81:\n')
        else:
            valid = True
    return int(selected_square)


def is_valid_selection(selected_square: int, current_dictionary: dict) -> bool:
    if current_dictionary[selected_square] != 0:
        valid_square_selection = False
    else:
        valid_square_selection = True
    return valid_square_selection


def value_selection(selected_square: int, current_dict: dict, master_dict: dict, current_grid: tuple) -> int:
    # Eventually, would like "0" to clear the selected square but no point right now
    selected_value = input(f'Select the value for square {selected_square} by typing an integer from 1 to 9:\nIf you would like to cancel your square selection, type "cancel":\n').lower()
    valid = False
    while not valid:
        if selected_value == '' or ' ' in selected_value:
            selected_value = input('That is not a valid input. Please enter an integer from 1 to 9:\n')
        elif selected_value == 'cancel':
            play_game(current_dict, master_dict, current_grid)
        elif selected_value < '1' or selected_value > '9' or len(selected_value) > 1:
            selected_value = input(f'{selected_value} is not a valid input. Please type "cancel" or enter an integer from 1 to 9:\n')
        else:
            valid = True
    return int(selected_value)


def selection_value_status(selected_square: int, selected_value: int, master_dictionary: dict) -> bool:
    if master_dictionary[selected_square] == selected_value:
        return True
    else:
        return False


def incorrect_answer(wrong_answers: int, selected_value: int) -> int:
    print(f'{selected_value} is incorrect')
    wrong_answers += 1
    return wrong_answers


def updating_current_grid(selected_square: int, selected_value: int, current_grid: tuple) -> tuple:
    if selected_square == 81:
        current_grid[8][8] = selected_value
    else:
        x = selected_square//9
        y = (x * 9) + 1
        z = selected_square - y
        current_grid[x][z] = selected_value
    return current_grid


def updating_current_dict(selected_square: int, selected_value: int, current_dictionary: dict) -> dict:
    current_dictionary[selected_square] = selected_value
    return current_dictionary


def is_game_over(wrong_answers: int, current_dictionary: dict, master_dictionary: dict, game_running: bool) -> bool:
    if current_dictionary == master_dictionary:
        print('Congratulations on solving the puzzle!\n')
        game_running = False
    # elif wrong_answers >= 15:
    #     print('You have made too many mistakes!\nBetter luck next time!\n')
    #     game_running = False
    else:
        game_running = True
    return game_running


def continue_playing(cont: bool) -> bool:
    keep_playing = input('Would you like to play again?\nPlease enter either "yes" or "no":\n').lower()
    valid = False
    while not valid:
        if keep_playing == '' or ' ' in keep_playing:
            keep_playing = input('That is not a valid input.\nPlease enter either "yes" or "no":\n').lower()
            valid = False
        elif keep_playing == 'yes':
            cont = True
            valid = True
        elif keep_playing == 'no':
            print('Thank you for playing!\n')
            cont = False
            valid = True
        else:
            keep_playing = input(f'{keep_playing} is not a valid input.\nPlease enter either "yes" or "no":\n')
            valid = False
    return cont


def game_setup() -> tuple:
    parameter = 0
    master_groups = grid_generator(parameter)
    current_grid = current_grid_generator(master_groups[0])
    valid_puzzle = is_valid_puzzle(master_groups)
    if not valid_puzzle:
        print('Error in "is_valid_puzzle"')
    # current_groups = current_group_generator(current_grid)
    index_groups = index_group_generator()
    master_dict = master_dictionary(master_groups[0])
    current_dict = current_dictionary(current_grid)
    index_dict = index_dictionary(index_groups)
    return (current_dict, master_dict, current_grid)


def play_game(current_dict: dict, master_dict: dict, current_grid: tuple):
    wrong_answers = 0
    game_running = True
    while game_running:
        valid_selection = False
        selected_value = 0
        selected_square = 0
        correct_selection = 0
        print_color_grid(current_dict, selected_square, valid_selection, selected_value, correct_selection)
        while not valid_selection:
            selected_square = square_selection()
            valid_selection = is_valid_selection(selected_square, current_dict)
            print_color_grid(current_dict, selected_square, valid_selection, selected_value, correct_selection)
            if not valid_selection:
                print(f'That square already has a {current_dict[selected_square]} in it.')
        selected_value = value_selection(selected_square, current_dict, master_dict, current_grid)
        correct_selection = selection_value_status(selected_square, selected_value, master_dict)
        if correct_selection:
            current_grid = updating_current_grid(selected_square, selected_value, current_grid)
            current_dict = updating_current_dict(selected_square, selected_value, current_dict)
            print_color_grid(current_dict, selected_square, valid_selection, selected_value, correct_selection)
            print('Correct!')
            time.sleep(2)
        else:
            print_color_grid(current_dict, selected_square, valid_selection, selected_value, correct_selection)
            wrong_answers += incorrect_answer(wrong_answers, selected_value)
            time.sleep(2)
        game_running = is_game_over(wrong_answers, current_dict, master_dict, game_running)
    return


def main():
    # num = start()
    # if num == 1:
    #     print('not finished')
    #     return
    # elif num == 2:
    cont = True
    while cont:
        setup_tuple = game_setup()
        play_game(setup_tuple[0], setup_tuple[1], setup_tuple[2])
        cont = continue_playing(cont)
    return


main()

# Easy puzzles on Sudoku.com:
# 36, 38, 38, 36, 38
# Medium puzzles on Sudoku.com:
# 31, 30, 30, 30, 30
# Hard puzzles on Sudoku.com:
# 28, 23, 23, 25, 28
# Expert puzzles on Sudoku.com:
# 23, 22, 23, 22, 22
# Evil puzzles on Sudoku.com:
# 22, 22, 22, 22, 22

# Apparently you can have a list of lists, and can change the values of each
# Knowing this, could clean up a lot of the row, column, and box stuff
# Still want to have a separate py file to generate grids and send them to notepad
# Then pull from that so don't have to generate grids here
# Furthermore, would like to use pygame to actually be able to play Sudoku