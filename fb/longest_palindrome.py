def longest_palindrome(string):
    """returns the longest palindrome in string """
    
    
    return string[1]
    
    
    
print(longest_palindrome("gyih"))
    
    
def is_palindrome(string, i=0, j=):
    """returns true if string is a palindrome else false"""
    i = 0
    j = len(string) - 1
    while i < j:
        if string[i] != string[j]:
            return False
        i += 1 
        j -= 1
    return True

print(is_palindrome("3213"))



%timeit is_palindrome("3213")
