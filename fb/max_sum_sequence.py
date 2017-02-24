def maxSequenceSum(seq, maxsize):
    """returns the sum of any subsequence of seq, length maxsize or less """

    l = len(seq)
    maxsum = max(seq)

    for size in range(2, maxsize+1):
        sum = 0
        for i in range(0, size):
            sum += seq[i]
        if sum > maxsum:
            maxsum = sum
        #print(sum)

        for start_index in range(1, l-size+1):
            sum += - seq[start_index-1] + seq[start_index+size-1]
            if sum > maxsum:
                maxsum = sum
            #print(str(sum)+ ' ' + str(seq[start_index-1]) + ' ' + str(seq[start_index+size-1]))

    return maxsum
# maxSequenceSum 

#    if maxsize > length(seq):
        #fail

def maxsum_subsequence(seq, maxsize):
    """returns the sum of any subsequence of seq, length maxsize or less """

    maximum = max_ending_here = 0

    for s in seq:
        max_ending_here = max(0, max_ending_here + s)
        maximum = max(max_ending_here, maximum)
        print(str(s)+' '+str(max_ending_here)+' '+str(maximum))

    return

L = [-100, 95, 86, 47]
k = 3
if (maxSequenceSum(L, k) == 228):
    print("success 1")
else:
    print("fail 1")

L = [40, 91, -68, -36, 24, -67, -32, -23, -33, -52]
k = 7
if (maxSequenceSum(L, k) == 131):
    print("success 2")
else:
    print("fail 2")


maxsum_subsequence(L, k)


from random import randint

n = 0
N = 7000
k1 = 1000
seq1 = []
while n < N:
    seq1.append(randint(-100, 100))
    n += 1

print(seq1)

print(maxSequenceSum(seq1, k1))
