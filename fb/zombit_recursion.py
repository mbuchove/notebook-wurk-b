def zombits(str_S):
    """returns the last time that there will be number of zombits represented by str_S
    str_S will represent an integer less than or equal to 10**25"""

    # the even numbers and odd numbers increase monotonically
    # so numbers are not repeated unless one occurrence is even and the other odd
    # even numbers always greater than either adjacent odd, so later occurrence is the odd time

    class tab:
        """holds values between calls to recursive function R(m,d)"""
        # here store the indices rather than the value
        n_even = None
        n_odd = None

    def R(m, d):
        """works recursively backward through R using a dictionary to cache terms
        m - the index of R to return
        d - the dictionary to store R terms """

        if m in d:  # don't calculate terms already found
            return d[m]
        else:
            if m % 2 == 1:  # odd
                n = long((m - 1) / 2)
                Rm = R(n - 1, d) + R(n, d) + 1

                if Rm == S:
                    # if you've found a match, this is the only odd match
                    tab.n_odd = m
                    # could end here, but returning would only return from this function

            else:  # even
                n = long(m / 2)
                Rm = R(n, d) + R(n + 1, d) + n

                if Rm == S:  # if you find a match, this is the only possible even match
                    tab.n_even = m

            d[m] = Rm  # set it in dictionary
            return Rm  # also return value

    S = long(str_S)  # long can store the whole number
    start_point = S + 2  # time always less than or equal to number of zombits - 1

    # initialize dictionary of R terms
    R_dict = dict()
    R_dict[0] = 1
    R_dict[1] = 1
    R_dict[2] = 2

    # search through odds first for match
    lower = 0
    upper = start_point
    while lower <= upper - 2:  # and not odd_found: # loop until match found then break
        if tab.n_odd:
            return str(tab.n_odd)
        middle = long((lower + upper) // 2)
        if middle % 2 == 0:
            middle += 1

        if R(middle, R_dict) == S:
            return str(middle)
        else:
            if S < R(middle, R_dict):
                upper = middle - 1
            else:
                lower = middle + 1
    # end search for odd match

    # then search through evens for match
    lower = 0
    upper = start_point
    while lower <= upper - 2:  # loop until match found then break
        if tab.n_even:
            return str(tab.n_even)

        middle = long((lower + upper) // 2)
        if middle % 2 == 1:
            middle -= 1

        if R(middle, R_dict) == S:
            return str(middle)
        else:
            if S < R(middle, R_dict):
                upper = middle - 1
            else:
                lower = middle + 1
    # end search through evens

    # if nothing was found, return "None"
    return "None"


if __name__ == "__main__":
    str_S1 = "7"
    a1 = zombits(str_S1)
    print(a1)
    if a1 == "4":
        print("passed test 1")
    else:
        print("failed test 1")

    str_S2 = "100"
    if zombit(str_S2) == "None":
        print("passed test 2")
    else:
        print("failed test 2")
