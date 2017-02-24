import math
import random
# to be run in python 3 for print function to work

def print_matrix(matrix):
    for row in reversed(matrix):
        print(row)

def binary_search_x(matrix, xmin, xmax, y, upperval):
    """find the edge 1 in the transition from 0s to 1s"""

    xmid = (int)(xmin+xmax)//2
    xlo = xmin
    xhi = xmax
    if upperval == 1:
        add = 1
    else:
        add = -1

    while matrix[y][xmid] == matrix[y][xmid+1]:
        if xmid == xmin or xmid == xmax:
            return xmid
        if matrix[y][xmid] == upperval:
            xhi = xmid
        else:
            xlo = xmid
        xmid = (int)(xlo+xhi)//2

    if matrix[y][xmid] == 1:
        return xmid
    elif matrix[y][xmid] == 0:
        return xmid + 1

    return -1 # should never happen

def binary_search_xlo(matrix, xmin, xmax, y):
    """find the edge 1 in the transition from 0s to 1s"""
    xmid = (int)(xmin+xmax)//2
    xlo = xmin
    xhi = xmax
    while matrix[y][xmid] == matrix[y][xmid+1]:
        if xmid == xmin or xmid == xmax:
            return xmid
        if matrix[y][xmid] == 1:
            xhi = xmid
        else:
            xlo = xmid
        xmid = (int)(xlo+xhi)//2

    if matrix[y][xmid] == 1:
        return xmid
    elif matrix[y][xmid] == 0:
        return xmid + 1

    return -1 # should never happen

def binary_search_xhi(matrix, xmin, xmax, y):
    """find the edge 1 in the transition from 0s to 1s"""
    xmid = (int)(xmin+xmax)//2
    xlo = xmin
    xhi = xmax
    while matrix[y][xmid] == matrix[y][xmid-1]:
        if xmid == xmin or xmid == xmax:
            return xmid
        if matrix[y][xmid] == 0:
            xhi = xmid
        else:
            xlo = xmid
        xmid = (int)(xlo+xhi)//2

    if matrix[y][xmid] == 1:
        return xmid
    elif matrix[y][xmid] == 0:
        return xmid - 1

    return -1 # should never happen



def check_quadrants(matrix):
    """efficient algorithm for finding any marked 1 in a grid """

    ysize = len(matrix)
    xsize = len(matrix[0])

    n = 1
    f = 2
    dx = 2
    dy = 2
    while dx > 1 and dy > 1:
        dx = int(math.ceil(float(xsize) / f))
        dy = int(math.ceil(float(ysize) / f))
        for i in range(0, xsize, dx):
            for j in range(0, ysize, dy):
                if matrix[j][i] == 1:
                    print( i, j )
                    return i, j
        n += 1
        f = 2 ** n

    print ("couldn't find")  # this should never happen
    return xsize - 1, ysize - 1



def find_box(matrix):
    """returns coordinates (as tuples) of top left and bottom right corners of box """

    ysize = len(matrix)
    xsize = len(matrix[0])

    for y in range(ysize):
        #print matrix[y]
        pass

    x, y = check_quadrants(matrix)

    #xbl = x
    #for i in range(x):
    #    if matrix[y][i] == 1:
    #        xbl = i
    #        break
    xbl = binary_search_xlo(matrix, 0, x, y)


    ybl = y
    for j in range(y):
        if matrix[j][x] == 1:
            ybl = j
            break

    xtr = x
    for i in range(xsize-1, x, -1):
        if matrix[y][i] == 1:
            xtr = i
            break
    xtr = binary_search_xhi(matrix, x, xsize-1, y)

    ytr = y
    for j in range(ysize - 1, y, -1):
        if matrix[j][x] == 1:
            ytr = j
            break


    return xbl, ybl, xtr, ytr




def create_matrix(w, h, xbl=0, ybl=0, xtr=0, ytr=0):
    """creates matrix of width w and height h
    with a box specified by its top-left and bottom-right coordinates
    which will be denoted by 1s"""

    matrix = []
    row = [0] * w
    for i in range(h):
        matrix.append([0]*w)


    for x in range(xbl, xtr+1):
        for y in range(ybl, ytr+1):
             matrix[y][x] = 1

    return matrix

def add_box_to_matrix(matrix, xbl, ybl, xtr, ytr):
    """add another box to a matrix by filling 1s to the appropriate coordinates"""
    overlap = False
    for x in range(xbl, xtr+1):
        for y in range(ybl, ytr+1):
            if matrix[y][x] == 1:
                overlap = True
            else:
                matrix[y][x] = 1

    return overlap



N = 20
w = random.randint(1, N)
h = random.randint(1, N)


xbl = random.randint(0, w - 1)
ybl = random.randint(0, h - 1)
xtr = random.randint(xbl, w - 1)
ytr = random.randint(ybl, h - 1)

print (w, h, xbl, ybl, xtr, ytr)
matrix = create_matrix(w, h, xbl, ybl, xtr, ytr)

x1, y1, x2, y2 = find_box(matrix)

for row in reversed(matrix):
    print(row)

print (xbl, ybl, xtr, ytr)
print (x1, y1, x2, y2)


assert(x1 == xbl)
assert(y1 == ybl)
assert(x2 == xtr)
assert(y2 == ytr)


# now add another box
xbl = random.randint(0, w - 1)
ybl = random.randint(0, h - 1)
xtr = random.randint(xbl, w - 1)
ytr = random.randint(ybl, h - 1)





#matrix = create_matrix(20, 20, )
overlap = add_box_to_matrix(matrix, xbl, ybl, xtr, ytr)

print_matrix(matrix)

print (xbl, ybl, xtr, ytr)

print(overlap)
