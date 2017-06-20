def binary_search(arr, val):
    """binary search array for value"""
    xmin = 0 
    xmax = len(arr) - 1
    while xmin < xmax:
        # like math.ceil 
        xmid = (xmin+xmax)//2 + ((xmin+xmax) % 2 > 0)
        if arr[xmid] < val:
            xmin = xmid
        elif arr[xmid] > val:
            xmax = xmid 
        else:
            return xmid
            
    if arr[xmin] == val:
        return xmin
    else:
        return None 
    