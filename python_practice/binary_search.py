def bins(arr, val):
    xmin = 0 
    xmax = len(arr) - 1
    while xmin < xmax:
        xmid = (xmin+xmax) // 2
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
    