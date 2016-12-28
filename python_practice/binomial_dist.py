import operator as op

def choose(n, k):
    k = min(k, n - k)
    if k == 0: return 1
    numer = reduce(op.mul, xrange(n, n - k, -1))
    denom = reduce(op.mul, xrange(1, k + 1))
    return numer//denom

def cdf(k, n, p):
    f = lambda i: choose(n,i) * p**i * (1-p)**(n-i)
    print sum(map(f, range(0, k+1)))


cdf(8, 20, 0.5)

import math
p = 120 # dollars / month
T = 15 # years
r = 0.04879 # interest
lump_sum = 0
for i in xrange(1, 12*T+1):
    lump_sum += p*math.exp(r*i/12)

print lump_sum

print 120*(math.exp(r*15)-1)/(math.exp(r/12)-1)