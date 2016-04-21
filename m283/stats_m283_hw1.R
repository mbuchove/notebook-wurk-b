# Exercise 1 and 2 

# initialize variables 
R1_bar <- 0.01
R2_bar <- 0.013
var1 <- 0.0061
var2 <- 0.0046
sd1 <- sqrt(v1)
sd2 <- sqrt(v2)

# define functional form of rho and optimize
rho_f <- function(x1) {(var2-x1^2*var1-(1-x1)^2*var2)/(2*x1*(1-x1)*sd1*sd2)}
curve(rho, from=0, to=1, xname="x1") # yname is not a graphical parameter 
print(optimize(rho, interval=c(0,1), maximum=TRUE))
# the exact answer from solving analytically, agrees with minimizer 
rho_min <- sqrt(v2/v1)
print(rho_min)

# create sequence of x1 and x2 values following constraint for no short sales
x1_seq <- seq(0, 1, 0.01)
x2_seq <- 1 - x1_seq

# create the points for return and risk for the portfolio 
Rp_bar <- x1_seq*R1_bar + x2_seq*R2_bar
var_p <- x1_seq^2*var1 + x2_seq^2*var2 + 2*x1_seq*x2_seq*rho_min*sd1*sd2
sd_p <- sqrt(var_p)

# plot 
plot(sd_p, Rp_bar, col="blue", xlab="Stdev of portfolio", ylab="expected return of portfolio")


# Exercise 4 
sig2_avg = 50 
cov_avg = 10 
sig2_p <- function(n) {sig2_avg/n+(n-1)*cov_avg/n}
n_vec <- c(5, 10, 20, 50, 100)
risk_vec = sig2_p(n_vec)
print(risk_vec)

# Exercise 5
rho5 <- 0.95
# now allow short sales
x1_ss <- seq(-2, 2, 0.01)
x2_ss <- 1 - x1_s
Rp_bar5 <- x1_ss*R1_bar + x2_ss*R2_bar
var_p5 <- x1_ss^2*var1 + x2_ss^2*var2 + 2*x1_ss*x2_ss*rho5*sd1*sd2
sd_p5 <- sqrt(var_p5)

# define return from 2 stocks 
Ret_p <- function(x1, x2, R1, R2) {x1*R1 + x2*R2}
risk_p <- function(x1, x2, v1, v2, cov) {sqrt(x1^2*var1 + x2^2*var2 +2*x1*x2*cov)}
x1_min <- function(v1, v2, cov) {(v2-cov)/(v1+v2-2*cov)} # found analytically 
# verify the minimum agrees with analytic solution 
minin = which.min(sd_p5)
xm = x1_ss[minin]
print(xm) # min in plot 
print(x1_min(var1, var2, rho5*sd1*sd2))
print(min(sd_p5))
print(risk_p(xm, 1-xm, var1, var2, rho5*sd1*sd2))

plot(sd_p5, Rp_bar5, col="blue", xlab="Stdev of portfolio", ylab="expected return of portfolio")
plot(x1_ss, sd_p5)

Rp_bar5_ns <- x1_seq*R1_bar + x2_seq*R2_bar
var_p5_ns <- x1_seq^2*var1 + x2_seq^2*var2 + 2*x1_seq*x2_seq*rho_min*sd1*sd2
sd_p5_ns <- sqrt(var_p5_ns)
points(sd_p5_ns, Rp_bar5_ns, col="green")


# Exercise 6 

# apple
a1 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)
# google 
a2 <- read.table("http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=00&b=01&c=2010&d=03&e=1&f=2015&g=m&ignore=.csv", sep=',', header=TRUE)

# calculate returns from adjusted close prices 
r1 <- (a1$Adj.Close[-64] - a1$Adj.Close[-1]) / (a1$Adj.Close[-1])
r2 <- (a2$Adj.Close[-64] - a2$Adj.Close[-1]) / (a2$Adj.Close[-1])

# create data frame  using the returns:
rr <- data.frame(r1, r2)

# compute the means:
means <- colMeans(rr)

#Find the covariance matrix:
cov.matrix <- cov(rr)

# make vector of 1s, one for each stock 
ones <- rep(1, 2)


# find the hyperbola parameters 
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


# Exercise 7 
returns = c(0.005174, 0.010617, 0.016947)
Rf = 0.001 # risk free asset 
rho7 = 0.0
R_vec = returns - Rf
cov_mat_7 = matrix( c(0.010025, 0, 0, 0, 0.002123, 0, 0, 0, 0.005775), nrow=3, ncol=3 )
cov_mat_7_inv = solve(cov_mat_7)
print(cov_mat_7_inv)
Z = cov_mat_7_inv %*% R_vec
lambda = sum(Z)
X_c = Z / lambda
print(t(cov_mat_7))


# Exercise 8 

returns = c(15, 12, 5, 9)
stdevs = c(36, 15, 7, 21)
plot(stdevs, returns)


