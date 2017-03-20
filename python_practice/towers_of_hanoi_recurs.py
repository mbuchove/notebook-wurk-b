
def towers_of_hanoi(n, src, dest, aux):
    """print moves to transfer n rings from one peg to another"""

    if n == 1:
        return [(src, dest)]

    front = towers_of_hanoi(n-1, src, aux, dest)
    mid = [(src, dest)]
    back = towers_of_hanoi(n-1, aux, dest, src)

    return front + mid + back



print towers_of_hanoi(4, 1, 3, 2)



# peg1 = list(range(1, n+1))
# peg2 = peg3 = []

# def recursive_call(n, src, dest, aux):
# """move n from src to dest with aux as intermediary"""
