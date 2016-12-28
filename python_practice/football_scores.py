
class perm_finder():

    ways = dict()
    ways[0] = 1


    def find_ways(this, n):

        if n in this.ways:
            return this.ways[n]

        tot = 0
        for i in (2, 3, 6, 7, 8):
            if i <= n:
                tot += this.find_ways(n-i)

        this.ways[n] = tot
        return tot


pf = perm_finder()
print pf.find_ways(35)



class perm_finder():
    """class for finding number of ways you can reach score if you can only score 3, 5, or 10"""
    ways = dict()
    ways[0] = 1

    def find_ways(this, n):
        """recursive call """
        if n in this.ways:
            return this.ways[n]

        tot = 0
        for i in (3, 5, 10):
            if i <= n:
                tot += this.find_ways(n-i)

        this.ways[n] = tot
        return tot
    # end find_ways
# end perm_finder class

pf = perm_finder()
print pf.find_ways(13)
print pf.ways