from math import factorial, floor

def perf_2_n_recurs(t, fun):
    """meant to be general, incomplete"""

    def rec(t, i, fun):
        if i > 0:
            dice_rolls[i] = 0
            rec(t, i - 1, fun)
            dice_rolls[i] = 1
            rec(t, i - 1, fun)
        else:  # i == 0
            count += fun(t, n)

    count = 0
    dice_rolls = [0] * t

    i = t - 1
    rec(t, i, n)


def upper_limit(t, n, b):
    """the maximum number of valid combination, ignoring rules that lose the game"""
    count = 0
    for N_1 in xrange(n-b, int(floor((t+n-b)/2 + 1))):
        N_n1 = N_1 + b - n
        N_0 = t - N_1 - N_n1
        count += factorial(N_1 + N_n1 + N_0)/(factorial(N_1)*factorial(N_n1)*factorial(N_0))
        print(N_1, N_n1, N_0)
    return count

print(upper_limit(5, 3, 1))


# upper_limit(t, n, b)

def test_all_outcomes(t, n):
    """t rolls, n spaces on board"""
    class count:
        c = 0

    def rec(t, n, b, h):
        """b is current board space, h is history list"""

        if b < 1 or b > n:
            return  # 0
        elif t == 0:
            # return 1 if b==n else return 0
            if b == n:
                count.c += 1
                print(h)
                return
            else:
                return  # 0
        else:
            h0 = list(h)
            h0.append(0)
            rec( t -1, n, b, h0)
            if b != n:
                hn1 = list(h)
                hn1.append(-1)
                rec( t -1, n, b- 1, hn1)
                h1 = list(h)
                h1.append(1)
                rec(t - 1, n, b + 1, h1)

    rec(t, n, 1, [])  #

    return count.c % 123454321


def answer(t, n):
    """t rolls, n spaces on board"""

    class dat:
        """data to hold through recursive calls"""
        rd = dict()
        nd = dict()
    # data

    def num_return(s, n, b):
        """return number of ways to return back to original spot"""
        from math import factorial
        if s == 1:
            return 1
        elif s == 2:
            c = 1
            if b > 1:
                c += 1
            if b < n:
                c += 1
            return c
        else: # filler
            return 1

        #free_rolls = s - n + b  # number of rolls that could be burned and still win
        #s = t - n + b
        max_pairs = (s - s % 2) // 2  # find min of this and allowable double forwards or double-backs
        max_back = b - 1
        max_forward = n - b
        safe_pairs = min(max_pairs, max_back, max_forward)
        count = 0
        for num_pairs in xrange(0, max_pairs + 1):
            num_zeros = s - 2 * num_pairs
            count += factorial(s) / (factorial(num_zeros) * factorial(num_pairs) ** 2)
        return count
    # num_return(s, n, b)


    def rec(t, n, b):
        """b is current board space"""


        # if success is no longer possible
        if b < 1 or b > n or t+b < n:
            return 0
        # successful end of game
        elif t + b == n:
            return 1
        # game over, failed
        elif t == 0:
            return 0
        # once end is reached, only way to succeed is to stay every roll
        elif b == n:
            return 1
        else: # first check dictionary for value
            if (t, n, b) in dat.rd:
                return dat.rd[(t, n, b)]
            else: # add number of possibilities for each of next roll
                free_rolls = t - n + b  # number of rolls that could be burned and still win
#                for s in

                v = rec(t - 1, n, b - 1) + rec(t - 1, n, b) + rec(t - 1, n, b + 1)
                dat.rd[(t, n, b)] = v # store value
                return v
    # rec(s, n, b):



    return rec(t, n, 1)  # % 123454321
# end answer(t, n)




   # return S % 123454321


#if answer(1, 2) == 1 print('passed') else print('failed')
#if answer(3, 2) == 3 print('passed') else print('failed')


t = 500
n = 250

print(answer(t, n))

def answer(t, n):
    """t rolls, n spaces on board"""





