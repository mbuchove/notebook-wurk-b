def obscure_by_height(x, y, n):
    """n is number of individuals of unique heights, 3 <= n <= 40
    return the number of ways to arrange n individuals in a line such that
    x are visible from one side and y are visible from the other, 1 <= x,y <= n"""

    # you can't see past the tallest one, so the only rabbit both guards can see is the tallest
    if x + y > n + 1:
        return "0"

    import math  # for faster factorial
    # like n choose k without the (n-k)! term in the denominator
    def factorial_fraction(num, den):
        """returns reduced factorial of num/den without performing full multiplication"""
        product = 1
        for n in range(den, num):
            product *= n + 1
        return product

    def choose(n, k):
        """calculates n choose k"""
        if 0 <= k <= n:
            nfact = 1
            kfact = 1
            for t in xrange(1, min(k, n - k) + 1):
                nfact *= n
                kfact *= t
                n -= 1
            return long(nfact // kfact)
        else:
            return 0

    class recurs_dict:
        """holds dictionary so recursions not repeated"""
        rd = dict()

    # typically would just use my cache decorator to memorize

    # @cache
    def recurs(x, nx):
        """recursively perform half of problem
        this problem is symmetric, so the same function can be used for y
        as long as the indices are called correctly"""

        if x < 1:  # this is the terminating case
            # return 1 instead of 0 because number of other possible combinations are being multiplied by this number
            return 1
        elif (x, nx) in recurs_dict.rd:
            return recurs_dict.rd[(x, nx)]
        elif x == 1:
            # total =  factorial_fraction(nx-1, 1) # denominator of 1 makes this just a factorial
            total = math.factorial(nx - 1)  # this implementation is faster because it calls a C subroutine, tested
            recurs_dict.rd[(x, nx)] = total
            return total
        else:
            total = 0
            for x1 in range(x, nx + 1):  # iterate over all potential positions x1 for the tallest bunny
                # for each position, we can look at the same problem for all the pieces to the west (east) of tallest
                # the number remaining to be visible from this side is now x - 1
                # all nx - x1 to the far side of the tallest will not be visible, and can be arranged any way
                total += recurs(x - 1, x1 - 1) * factorial_fraction(nx - 1, x1 - 1)
            recurs_dict.rd[(x, nx)] = total
            return total
            # end def recurs(x, nx)

    # initialize count of combinations
    combinations = 0

    # iterate over possible placements of the tallest bunny
    # multiply the ways the right could be arranged by the number of ways the left could be arranged
    # they're independent because you can't see over the
    for i in range(x, n - y + 2):
        # if guard is only supposed to see 1 from either side, the tallest one must be on that end
        if not (x == 1 and i != 1) and not (y == 1 and i != n):
            combinations += recurs(x - 1, i - 1) * recurs(y - 1, n - i) * choose(n - 1, i - 1)

    # return the number of possible combinations as a string
    return str(combinations)

# end answer(x, y, n)

#% timeit - n
#100
#answer(1, 20, 40)
