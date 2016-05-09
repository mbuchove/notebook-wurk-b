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



def answer(seq):
    """returns a string representing the number (in base-10) of sequences that would result in the same tree as the given sequence"""
    root_node = bun_node(seq[0])
    for b in seq[1:]:
        add_bunny(age, root_node)


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
    root_main = build_tree(seq)

    num_matches = 0
    for perm in permutations(seq):
        perm_root = build_tree(perm)
        if compare_trees(root_main, perm_root):
            num_matches += 1

    return str(num_matches)



for seq in [1, 2, 8, 7, 6, 4, 5, 9]:
    if not answer(seq) == bruteforce(seq):
        print("failure")
        print(seq)
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

