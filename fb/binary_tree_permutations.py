class bun_node:
    """class for node in binary tree"""
    def __init__(self, age, left=None, right=None):
        """instantiate node: left is younger, right is older"""
        self.age = age
        self.left = left
        self.right = right

# class bun_node


def add_bunny(age, root):
    """add a bunny node to the tree"""
    while root:
        if age < root.age:
            if root.left:
                root = root.left
            else:
                root.left = bun_node(age)
                return
        elif age > root.age:
            if root.right:
                root = root.right
            else:
                root.right = bun_node(age)
                return
# add bunny

def build_tree(seq):
    """build a tree from the sequence, return the root node"""
    root_node = bun_node(seq[0])
    for age in seq[1:]:
        add_bunny(age, root_node)
    return root_node



def equiv_perms_bin(seq):
    """find number of permutations of sequence that create the same binary tree
    return result as a string"""
    from math import factorial

    def recurs(seq):
        """recursive call that returns ints"""
        if len(seq) < 3:
            return 1 # there is only one way to arrange sequences 2 or less
        else:
            # first split into left side and right side - exchanging order of left and right sequence doesn't make a difference
            left_seq = [x for x in seq if x < seq[0]]
            right_seq = [x for x in seq if x > seq[0]]
            nl = len(left_seq)
            nr = len(right_seq)
            # the number of combinations is n choose k multiplied by the number of ways each sub tree could be arranged
            # n choose k is the number of ways you can exchange the left and right numbers while preserving the order of left and right
            # then multiply by the number of valid ways to arrange each sequence by making a recursive call
            combos = factorial(nl + nr) * recurs(left_seq) / factorial(nl) * recurs(right_seq) / factorial(nr)

            return int(combos)

    # return string after recursive calls complete
    return str(recurs(seq))


        # keep scrolling as long as you are incrementing by 1 continuously or decrementing by 1 continuously

        # if subset is entirely to one side of root (all left or all right) then it acts as its own sequence with the first entry being the root node

        # if you increment or decrement by 1,




def compare_trees(root_A, root_B):
    """compares trees specified by their root nodes
    returns true if they're equal, false if they differ"""
    if root_A == root_B:
        return True
    elif not root_A or not root_B:
        return False
    elif root_A.age != root_B.age:
        return False
    else:
        return compare_trees(root_A.left, root_B.left) and compare_trees(root_A.right, root_B.right)



from itertools import permutations
def bruteforce(seq):
    """bruteforce solution for above answer"""
    if len(seq) == 0:
        return 0
    root_main = build_tree(seq)

    num_matches = 0
    for perm in permutations(seq):
        #print(perm)
        perm_root = build_tree(perm)
        if compare_trees(root_main, perm_root):
            num_matches += 1
            print(perm)

    return str(num_matches)

#for perm in permutations([1,5,3,7]):
#    print(perm)
print(bruteforce([2, 1, 3])) # returns 2
#print(bruteforce([1, 2, 8, 7, 6, 4, 5, 9]))
print(answer( [5, 9, 8, 2, 1, 3 ] ))
print(answer([5,7,6,8,9,2,1,3,4]))
print(answer([9, 17, 22, 41, 5, 13, 31, 34, 39, 35, 10, 45, 26, 0, 30, 1, 3, 27, 11, 44, 24, 36, 33, 8, 16, 29, 6, 38, 2, 43, 40, 49, 47, 48, 20, 18, 19, 21, 23, 28, 15, 42, 37, 4, 7, 32, 25, 46, 12, 14]))



if False:
    for seq in [[1, 2, 8, 7, 6, 4, 5, 9],]:
        if not answer(seq) == bruteforce(seq):
            print("failure")
            print(seq)
        #else:
            #print("passed")
            #print(seq)
    print("complete!")


    # test the implementations
    seq = [5, 9, 8, 2, 1]
    if bruteforce(seq) == "6":
        print("test 1 passed")
    else:
        print("test 1 failed")

    if bruteforce([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) == "1":
        print("test 2 passed")
    else:
        print("test 2 failed")

# end run tests
