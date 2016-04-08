#Data:
a1 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

a2 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

a3 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=XOM&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

#Convert the adjusted close prices into returns:
r1 <- (a1$Adj.Close[-64] - a1$Adj.Close[-1]) / (a1$Adj.Close[-1])
r2 <- (a2$Adj.Close[-64] - a2$Adj.Close[-1]) / (a2$Adj.Close[-1])
r3 <- (a3$Adj.Close[-64] - a3$Adj.Close[-1]) / (a3$Adj.Close[-1])

#Create a data frame  using the returns:
rr <- data.frame(r1, r2, r3)

#Compute the means:
means <- colMeans(rr)

#Find the covariance matrix:
cov.matrix <- cov(rr)

ones <- rep(1, 3)
A <- t(ones) %*% solve(cov.matrix) %*% means

B <- t(means) %*% solve(cov.matrix) %*% means

C <- t(ones) %*% solve(cov.matrix) %*% ones

D <- B*C - A^2


plot(0, A/C, main = "Portfolio possibilities curve", xlab = "Risk",
     ylab = "Expected Return", type = "n",
     xlim = c(-2*sqrt(1/C), 4*sqrt(1/C)), 
     ylim = c(-2*A/C, 4*A/C))

#Plot center of the hyperbola:
points(0, A/C, pch = 19)

#Plot transverse and conjugate axes:
abline(v = 0) #Also this is the y-axis.
abline(h = A/C)

#Plot the x-axis:
abline(h = 0)

#Plot the minimum risk portfolio:
points(sqrt(1/C), A/C, pch=19)

#Find the asymptotes:
V <- seq(-1, 1, 0.001)
A1 <- A/C + V * sqrt(D/C)
A2 <- A/C - V * sqrt(D/C)
points(V, A1, type = "l")
points(V, A2, type = "l")

#Efficient frontier:
minvar <- 1/C
minE <- A/C
sdeff <- seq((minvar)^0.5, 1, by = 0.0001)
options(warn = -1)
y1 <- (A + sqrt(D*(C*sdeff^2 - 1)))*(1/C) 
y2 <- (A - sqrt(D*(C*sdeff^2 - 1)))*(1/C) 
options(warn = 0)

points(sdeff, y1, type = "l")
points(sdeff, y2, type = "l")

#====================================================================================
#Another way to trace out the efficient frontier:
#Use combinations of two portfolios on the efficient frontier.

#Read the data:
a1 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

a2 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

a3 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=XOM&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

#Convert the adjusted close prices into returns:
r1 <- (a1$Adj.Close[-64] - a1$Adj.Close[-1]) / (a1$Adj.Close[-1])
r2 <- (a2$Adj.Close[-64] - a2$Adj.Close[-1]) / (a2$Adj.Close[-1])
r3 <- (a3$Adj.Close[-64] - a3$Adj.Close[-1]) / (a3$Adj.Close[-1])

#Create a data frame using the returns:

#Compute the means:

#Find the covariance matrix:


#Need to find the composition of two points on the efficient frontier:

#First point (A):  Minimum risk portfolio.
#Find the inverse of the var-covar matrix.  Call it Sigmainv,

#One of the two portfolios will be the minimum risk portfolio.  Find its composition:

#Place the weights of the minimum risk portfolio into a vector.  Call it xa:


#Second point (B):  Use Rf=0.001.
#Find Zb:
#1.  Find R:

#2.  Find Zb:

#3.  Find xb:

#Compute the mean return and variance of A:


#Compute the mean return and variance of B:


#Compute the covariance between A and B:


#Finally, use many combinations of the two portfolios to trace out the efficient frontier:
#Here are the combinations:
a <- seq(-2, 2, .01)
b <- 1-a

#Compute the mean return for each combination:


#Compute the standard deviation for each combination:


#Plot the efficient frontier and add the two efficient portfolios A and B:
