from collections import namedtuple
from operator import mul

try:
    reduce = reduce
except NameError:
    from functools import reduce # py3k

Info = namedtuple('Info', 'start height')

def max_size(mat, value=0):
    """Find height, width of the largest rectangle containing all `value`'s.

    For each row solve "Largest Rectangle in a Histrogram" problem [1]:

    [1]: http://blog.csdn.net/arbuckle/archive/2006/05/06/710988.aspx
    """
    it = iter(mat)
    hist = [(el==value) for el in next(it, [])]
    max_size = max_rectangle_size(hist)
    for row in it:
        hist = [(1+h) if el == value else 0 for h, el in zip(hist, row)]
        max_size = max(max_size, max_rectangle_size(hist), key=area)
    return max_size
# max_size

def max_rectangle_size(histogram):
    """Find height, width of the largest rectangle that fits entirely under
    the histogram.

    >>> f = max_rectangle_size
    >>> f([5,3,1])
    (3, 2)

    Algorithm is "Linear search using a stack of incomplete subproblems" [1].
    """

    print(histogram)
    stack = []
    top = lambda: stack[-1]
    max_size = (0, 0) # height, width of the largest rectangle
    pos = 0 # current position in the histogram
    for pos, height in enumerate(histogram):
        start = pos # position where rectangle starts
        while True:
            print(stack)
            if not stack or height > top().height:
                stack.append(Info(start, height)) # push
            elif stack and height < top().height:
                max_size = max(max_size, (top().height, (pos - top().start)),
                               key=area)
                start, _ = stack.pop()
                continue
            break # height == top().height goes here

    pos += 1
    for start, height in stack:
        max_size = max(max_size, (height, (pos - start)), key=area)

    return max_size
# max_rectangle_size

def area(size):
    return size[0]*size[1]
    #return reduce(mul, size)
# area

import unittest
class TestCase(unittest.TestCase):
    def test(self):
        self.assertEqual(max_size(self.__s2m("""
        0 0 0 0 1 0
        0 0 1 0 0 1
        0 0 0 0 0 0
        1 0 0 0 0 0
        0 0 0 0 0 1
        0 0 1 0 0 0""")), (3, 4))
        self.assertEqual(max_size([[1, 1], [0, 0]]), (1, 2))
        self.assertEqual(max_size([[0, 0], [1, 1]]), (1, 2))
        self.assertEqual(max_size([[1, 0], [1, 0]]), (2, 1))
        self.assertEqual(max_size([[0, 1], [0, 1]]), (2, 1))
        self.assertEqual(max_size(self.__s2m("""
        0 0 0 0 1 0
        0 0 1 0 0 1
        0 0 0 0 0 0
        1 0 0 0 0 0
        0 0 0 0 0 1
        0 0 1 0 0 0
        0 0 0 0 0 0
        0 0 0 0 0 0""")), (7, 2))
        self.assertEqual(max_size([[]]), (0, 0))
        self.assertEqual(max_size([]), (0, 0))
        self.assertEqual(max_size(self.__s2m("""
        0 0 0 0 1 0
        0 0 1 0 0 1
        0 0 0 0 0 0
        1 0 0 0 0 0
        0 0 0 0 0 0
        0 0 1 0 0 1
        0 0 0 0 0 0
        0 0 0 0 0 0""")), (3, 5))
        self.assertEqual(max_size(self.__s2m("""
        0 0 0 0 1 0
        0 0 0 0 0 0
        0 0 1 0 0 1
        0 0 0 0 0 0
        1 0 0 0 0 0
        0 0 0 0 0 0
        0 0 1 0 0 1
        0 0 0 0 0 0
        0 0 0 0 0 1""")), (8, 2))
        self.assertEqual(max_size(self.__s2m("""
        0 0 0 0 1 1 1
        0 0 0 0 0 0 0
        0 0 0 1 1 1 1
        0 0 1 1 1 1 1
        1 0 1 1 1 1 1
        1 0 1 1 1 1 1
        1 0 1 1 1 1 1
        """)), (3, 3))
    # test

    def __s2m(self, s):
        return [map(int, line.split())
                for line in s.splitlines() if line.strip()]
# TestCase

if __name__=="__main__":
    import unittest; unittest.main()
