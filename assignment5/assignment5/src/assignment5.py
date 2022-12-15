from typing import List
from functools import reduce
import sys
import traceback

#################
### Problem  1 ##
#################


def assoc_list (l):

    occs = {}

    for x in l:
        if x not in occs:
            occs[x] = 1
        else:
            occs[x] = occs[x] + 1

    return [(val, freq) for val, freq in occs.items()]


#################
### Problem  2 ##
#################


def buckets (f, l):

    res = []

    def add_eqv (fun, val, lst):
        for eql in lst:
            if (fun)(eql[0], val):
                eql.append(val)
                return
        lst.append([val])   
            
    for x in l:
        add_eqv (f, x, res)

    return res


###################################
# Definition for a binary tree node
###################################


class TreeNode:
    def __init__(self, val=None, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    # construct tree from a list of values `ls`
    def list_to_tree(self, ls):
        self.val = self.left = self.right = None # clear the current tree

        if not ls: # ls is None or l == []
            return # tree is None

        i = 0
        self.val = ls[i]
        queue = [self]
        while queue: # while queue is not empty
            i += 1
            node = queue.pop(0)
            if node.val is None:
                continue

            if 2*i -1 >= len(ls) or ls[2*i-1] is None:
                pass
            else:
                node.left = TreeNode(ls[2*i-1])
                queue.append(node.left)

            if 2*i >= len(ls) or ls[2*i] is None:
                pass
            else:
                node.right = TreeNode(ls[2*i])
                queue.append(node.right)


#################
### Problem  3 ##
#################


def level_order(root: TreeNode):

    res = []

    if root == None:
        return res

    queue = [root]

    while (len(queue) != 0):    
        curr_level = []
        q_size = len(queue)

        for i in range(q_size):
            curr_node = queue.pop(0) 
            curr_level.append(curr_node.val)
            if (curr_node.left): queue.append(curr_node.left)
            if (curr_node.right): queue.append(curr_node.right)
            
        res.append(curr_level)
    
    return res


#################
### Problem  4 ##
#################


def pathSum(root: TreeNode, targetSum: int) -> List[List[int]]:

    res = []
    stack = []

    def dfs_helper (node: TreeNode, sum: int, p_stack):

        if node == None:
            return

        curr_val = node.val
        sum += curr_val
        stack.append(curr_val)

        if (not node.left) and (not node.right) and (sum == targetSum):
            res.append(list(stack))
        else:
            dfs_helper(node.left, sum, p_stack)
            dfs_helper(node.right, sum, p_stack)

        stack.pop()
                    
    dfs_helper(root, 0, stack)

    return res


#################
### Test cases ##
#################

def main():
    print ("Testing your code ...")
    error_count = 0

    # Testcases for Problem 1
    try:
        result = assoc_list([1, 2, 2, 1, 3])
        result.sort(key=lambda x:x[0])
        assert (result == [(1,2), (2, 2), (3, 1)])

        result = assoc_list(["a","a","b","a"])
        result.sort(key=lambda x:x[0])
        assert (result == [("a",3), ("b",1)])

        result = assoc_list([1, 7, 7, 1, 5, 2, 7, 7])
        result.sort(key=lambda x:x[0])
        assert (result == [(1,2), (2,1), (5,1), (7,4)])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 2
    try:
        assert (buckets (lambda a, b : a == b, [1,2,3,4]) == [[1], [2], [3], [4]])
        assert (buckets (lambda a, b : a == b, [1,2,3,4,2,3,4,3,4]) == [[1], [2, 2], [3, 3, 3], [4, 4, 4]])
        assert (buckets (lambda a, b : a % 3 == b % 3, [1,2,3,4,5,6]) == [[1, 4], [2, 5], [3, 6]])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    ### Specify 3 trees for testing problems 3 & 4
    root_1 = TreeNode()
    root_1.list_to_tree([5,4,8,11,None,13,4,7,2,None,None,5,1])

    root_2 = TreeNode()
    root_2.list_to_tree([1,2,3])

    root_3 = TreeNode()
    root_3.list_to_tree([1,2])

    # Testcases for Problem 3
    try:
        assert (level_order(root_1) == [[5], [4, 8], [11, 13, 4], [7, 2, 5, 1]])
        assert (level_order(root_2) == [[1], [2, 3]])
        assert (level_order(root_3) == [[1], [2]])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 4
    try:
        assert (pathSum(root_1, 22) == [[5, 4, 11, 2], [5, 8, 4, 5]])
        assert (pathSum(root_2, 4) == [[1, 3]])
        assert (pathSum(root_3, 0) == [])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    print (f"{error_count} out of 4 programming questions are incorrect.")

main()
