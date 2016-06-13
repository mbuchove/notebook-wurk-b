
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
        rec = True
        rd = dict()

    def rec(t, n, b):
        """b is current board space"""

        if b < 1 or b > n or t+b < n:
            return 0
        elif t + b == n:
            return 1
        elif t == 0:
            return 0
        elif b == n:
            return 1
        else:
            if (t, n, b) in dat.rd and dat.rec:
                return dat.rd[(t, n, b)]
            else:
                v = rec(t - 1, n, b - 1) + rec(t - 1, n, b) + rec(t - 1, n, b + 1)
                if dat.rec:
                    dat.rd[(t, n, b)] = v
                return v

    if dat.rec:
        print(dat.rd)
    return rec(t, n, 1)  # % 123454321
    # end answer(t, n)




   # return S % 123454321


#if answer(1, 2) == 1 print('passed') else print('failed')
#if answer(3, 2) == 3 print('passed') else print('failed')


t = 500
n = 250

print(answer(t, n))
