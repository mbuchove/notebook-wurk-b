"""Towers of Hanoi"""

import copy
from collections import deque

def towers_of_hanoi(start, end):
    """
    Finds a path from start to end using BFS
    """
    queue = deque()
    explored = []
    queue.append([start])
    explored.append(start)
    while queue:
        path = queue.popleft()
        state = path[-1]
        if state == end:
            return path
        for next_state in successors(state):
            if next_state not in explored:
                new_path = list(path)
                new_path.append(next_state)
                queue.append(new_path)
                explored.append(next_state)
    return None

def successors(state):
    """
    Returns the possible next states after state in a list
    """
    next_states = []
    for peg in state:
        if state[peg]:
            disk = state[peg].pop()
            for other_peg in state:
                if peg != other_peg:
                    if len(state[other_peg]) == 0 or state[other_peg][-1] > disk:
                        state[other_peg].append(disk)
                        next_states.append(copy.deepcopy(state))
                        disk = state[other_peg].pop()
            state[peg].append(disk)
    return next_states

def pretty_print(state_list, gap):
    """
    Prints the path in a nice aligned format
    """
    for state in state_list:
        s_1 = ' ' * (gap - len(str(state['1'])) + 1)
        s_2 = ' ' * (gap - len(str(state['2'])) + 1)
        s_3 = ' ' * (gap - len(str(state['3'])) + 1)
        print('1: {}'.format(state['1']) + s_1 + '2: {}'.format(state['2']) +
              s_2 + '3: {}'.format(state['3']) + s_3)

def towers_of_hanoi_recursive(num_rings):
    """
    Towers of Hanoi recusive solution
    """
    pegs = [
        [i for i in range(num_rings, 0, -1)],
        [],
        []
    ]
    _towers_of_hanoi_recursive(num_rings, pegs, 0, 1, 2)

def _towers_of_hanoi_recursive(num_rings, pegs, from_peg, to_peg, use_peg):
    if num_rings > 0:
        _towers_of_hanoi_recursive(num_rings - 1, pegs, from_peg, use_peg, to_peg)
        pegs[to_peg].append(pegs[from_peg].pop())
        print('Move from peg ', from_peg, ' to peg ', to_peg)
        # input()
        _towers_of_hanoi_recursive(num_rings - 1, pegs, use_peg, to_peg, from_peg)

def tests():
    """
    Unit tests
    """
    result = successors({'1': [5, 4, 3, 2, 1], '2': [], '3': []})
    assert len(result) == 2
    assert {'1': [5, 4, 3, 2], '2': [1], '3': []} in result
    assert {'1': [5, 4, 3, 2], '2': [], '3': [1]} in result
    result = successors({'1': [5, 4, 3, 2], '2': [1], '3': []})
    assert len(result) == 3
    assert {'1': [5, 4, 3], '2': [1], '3': [2]} in result
    assert {'1': [5, 4, 3, 2], '2': [], '3': [1]} in result
    assert {'1': [5, 4, 3, 2, 1], '2': [], '3': []} in result
    result = successors({'1': [5, 4], '2': [3], '3': [2, 1]})
    assert len(result) == 3
    assert {'1': [5, 4, 3], '2': [], '3': [2, 1]} in result
    assert {'1': [5, 4], '2': [3, 1], '3': [2]} in result
    assert {'1': [5, 4, 1], '2': [3], '3': [2]} in result
    print('Tests pass!')

if __name__ == "__main__":
    # tests()
    START = {'1': [6, 5, 4, 3, 2, 1], '2': [], '3': []}
    END = {'2': [6, 5, 4, 3, 2, 1], '3': [], '1': []}
    RESULT = towers_of_hanoi(START, END)
    print('Graph search result:\n')
    pretty_print(RESULT, len(str(START['1'])))
    print('\nRecursive result:\n')
    towers_of_hanoi_recursive(6)
