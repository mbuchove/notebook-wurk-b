# Exercise 1 

# read the table 
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c183_c283/statc183c283_5stocks.txt", header=T)
data_len = length(a$date)

# a. find returns:
# Exxon-Mobil
r1 <- (a$P1[-data_len] - a$P1[-1]) / (a$P1[-1])
# General Motors
r2 <- (a$P2[-data_len] - a$P2[-1]) / (a$P2[-1])
# Hewlett Packard
r3 <- (a$P3[-data_len] - a$P3[-1]) / (a$P3[-1])
# McDonalds
r4 <- (a$P4[-data_len] - a$P4[-1]) / (a$P4[-1])
# Boeing
r5 <- (a$P5[-data_len] - a$P5[-1]) / (a$P5[-1])

# b. create data frame using the returns, then find means and covariance, then print
returns <- data.frame(r1, r2, r3, r4, r5)
Rbar <- colMeans(returns)
cov.mat <- cov(returns)
print("1a. mean returns:")
print(Rbar)
print("covariance matrix")
print(cov.mat)

# c. make sub-matrix and vectors for just Exxon and Boeing
indices <- c(1, 5)
Rbar_15 <- Rbar[indices]
cov_15 <- cov.mat[indices, indices]
icov_15 <- solve(cov_15)

ones <- rep(1, 2)
# find the hyperbola parameters 
A <- (t(ones) %*% icov_15 %*% Rbar_15)[1] # extracts just value 
B <- (t(Rbar_15) %*% icov_15 %*% Rbar_15)[1]
C <- (t(ones) %*% icov_15 %*% ones)[1]
D <- B*C - A^2

#Efficient frontier:
minvar <- 1/C
minE <- A/C
print(cov_15)
print(cov_15[2, 2])
print(max(1, 2))
print(sqrt(max(cov_15[1, 1], cov_15[2, 2])))
# only plot to risk of individual assets to be consistent with no shortselling
sdeff1 <- seq((minvar)^0.5, sqrt(max(cov_15[1, 1], cov_15[2, 2])), by = 0.0001)
sdeff2 <- seq((minvar)^0.5, sqrt(min(cov_15[1, 1], cov_15[2, 2])), by = 0.0001)
y1 <- (A + sqrt(D*(C*sdeff1^2 - 1)))*(1/C) 
y2 <- (A - sqrt(D*(C*sdeff2^2 - 1)))*(1/C) 

plot(0, A/C, main = "Portfolio Possibilities Curve", xlab = "Risk",
     ylab = "Expected Return", type = "n",
     xlim = c(0, 4*sqrt(1/C)), 
     ylim = c(-2*A/C, 4*A/C))
points(sdeff1, y1, type="l", col="green")
points(sdeff2, y2, type="l", col="red")
#xlim = c(-2*sqrt(1/C), 4*sqrt(1/C)), 

# find the minimum risk portfolio 
x_min <- icov_15 %*% ones / as.numeric(t(ones) %*% icov_15 %*% ones)
print("minimum risk x:")
print(x_min)
risk_min = sqrt(1/C)
R_minrisk = A / C
print("minimum risk (standard deviation):")
print(risk_min)
print("return at minimum risk:")
print(R_minrisk)
points(risk_min, R_minrisk, pch=19, col="black")


# e. make sub-matrix and vectors for just these 3 stocks 
indices <- c(1, 4, 5)
Rbar_145 <- Rbar[indices]
cov_145 <- cov.mat[indices, indices]

# iterate over different combinations of stocks and append to a data frame  
xs <- read.table("http://www.stat.ucla.edu/~nchristo/datac183c283/statc183c283_abc.txt", header=T)
num_points = length(xs$"a")
df = data.frame(Eret=double(num_points), SD=double(num_points))
i <- 1
r_s <- apply(xs, 1, function(row) {
  df$Eret[i] <<- (t(row) %*% Rbar_145)[1]
  df$SD[i] <<- sqrt(t(row) %*% cov_145 %*% row)[1]
  i <<- i + 1 # double arrow makes it accessible outside of function 
})

print(min(df$Eret))
print(max(df$Eret))
# now plot the points 
plot(-0.002595035, 0.0155, main="Portfolio Possibilities Curve", 
     xlab="standard deviation", 
     ylab="Expected Return", 
     type ="n", 
     xlim=c(min(df$SD), max(df$SD)), 
     ylim=c(min(df$Eret), max(df$Eret)))
points(df$SD, df$Eret, pch=20)


# f. 
R_f <- 0.001

# for 3 stocks from e. 
icov_145 <- solve(cov_145)
R_rf <- Rbar_145 - R_f
Z <- icov_145 %*% R_rf
lambda <- sum(Z)
x_G <- Z / lambda
R_G <- ( t(x_G) %*% Rbar_145 )[1]
risk_G <- sqrt(( t(x_G) %*% cov_145 %*% x_G )[1])
print("point of tangency for part e.")
print("composition of G:")
print(x_G)
print("risk of G:")
print(risk_G)
print("return of G:")
print(R_G)

slope <- (R_G - R_f) / risk_G
tangent <- function(risk) {
  slope * risk + R_f
}

plot(tangent, -0.01, 0.2, 
     main="Portfolio Possibilities Curve", 
     xlab="standard deviation", 
     ylab="Expected Return", 
     type ="l", 
     ylim=c(-0.004, 0.019),
     col="green")
points(df$SD, df$Eret, pch=20)
points(risk_G, R_G, col="blue", pch=19)

# g. 
x_G <- 0.6
x_f <- 1.0 - x_G
R_p <- x_G * R_G + x_f * R_f
risk_p <- x_G * risk_G 
print("risk and return of point f on CAL:")
print(risk_p)
print(R_p)
points(risk_p, R_p, col="yellow", pch=19)


# h. 

R_f2 <- 0.002
Rbar_B <- Rbar_145 - R_f2
Z_B <- icov_145 %*% Rbar_B
lambda_B <- sum(Z_B)
x_B <- Z_B / lambda_B
R_B <- ( t(x_B) %*% Rbar_145 )[1]
risk_B <- sqrt( t(x_B) %*% cov_145 %*% x_B )[1]
print("risk of B:")
print(risk_B)
print("return of B:")
print(R_B)

# x_G -> x_A 
cov_AB = t(x_B) %*% cov_145 %*% x_B
print("covariance of A and B:")
print(cov_AB)
points(risk_B, R_B, col="orange", pch=19)

# calculate the minimum risk portfolio 
ones <- rep(1,length(x_B))
x_min <- icov_145 %*% ones / as.numeric(t(ones) %*% icov_145 %*% ones)
R_min <- ( t(x_min) %*% Rbar_145 )[1]
risk_min <- sqrt( t(x_min) %*% cov_145 %*% x_min )[1]
points(risk_min, R_min, col="red", pch=19)

# construct efficient frontier from A and B 




# Exercise 2 
R_f <- 0.04
R_A <- 0.12
sig_A <- 0.2
sig_B <- 0.08
rho <- 0.1
sr <- sqrt(rho*sig_A*sig_B)
# a. 
R_B <- ( R_A*(sig_B+sr) + R_f*(sig_A-sig_B) ) / (sig_A + sr)
print(R_B)
# b. 
R_B <- R_f + (R_A-R_f)*sr
print(R_B) 

# Exercise 3 
# a. 
rho <- (0.525 - 0.5*0.16 - 0.5*0.25) / (2*0.4*0.5)
print(rho)
# b. 
R_p <- 0.11
R_f <- 0.05
R_G <- ( t(c(0.6, 0.4)) %*% c(0.14, 0.1) )[1]
x_G <- (R_p - R_f) / (R_G - R_f)
print(x_G)
# b. 
R_p <- 0.10
x_G <- (R_p - R_f) / (R_G - R_f)
print(1-x_G)


# Exercise 5 
# a. 
ones <- rep(1, 2)
iQ <- matrix(c(166.21139, -22.40241, -22.40241, 220.41076), nrow=2, ncol=2)
Q <- solve(iQ)
x_min <- iQ %*% ones / as.numeric(t(ones) %*% iQ %*% ones)
print(x_min)
# b. 
R_f <- 0.011
sd_A <- sqrt((t(x_min) %*% Q %*% x_min)[1])
R_A <- 0.01315856
R_B <- 0.01219724
# x_f * R_f + (1-x_f) * R_A = R_B
x_f <- ( R_A - R_B ) / (R_A - R_f)
print(x_f)



# Exercise 6 
# single index variance
beta_A = 0.79
beta_B = 1.12
sig_eps_A = 0.027
sig_eps_B = 0.006
sig_m = sqrt(0.0022)
var_si <- function(beta, sig_eps, sig_m) {
  beta^2*sig_m^2 + sig_eps^2
}
var_A <- var_si(beta_A, sig_eps_A, sig_m)
var_B <- var_si(beta_B, sig_eps_B, sig_m)
rho <- beta_A*beta_B*sig_m^2/(sqrt(var_A)*sqrt(var_B))




# extra test code 

# for all 5 stocks 
#inv_cov <- solve(cov.mat)
#Rbar_rf <- Rbar - R_f
#Z <- inv_cov %*% Rbar
#lambda <- sum(Z)
#x_G <- Z / lambda
#print(x_G)
#R_G <- ( t(x_G) %*% Rbar )[1][1]
#risk_G <- ( t(x_G) %*% cov.mat %*% x_G )[1][1]
#print("risk of G:")
#print(risk_G)
#print("return of G:")
#print(R_G)

#abline(tangent)
#plot?

options(warn = -1)
options(warn = 0)

#print(double(10))

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

#install.packages(microbenchmark)
#library(microbenchmark)
#microbenchmark(f1(1000), f2(1000), f3(1000), times=5)

system.time(sqrt(1000))

#dfx = data.frame(ev1=1:10, ev2=sample(10:99, 10), ev3=10:1)
#with(dfx, symbols(x=ev1, y=ev2, circles=ev3, inches=1/3, ann=F, bg="steelblue2", fg=NULL))


#typeof(a)

#typeof(a$P1)
a1 = a$P1
#print(a1)

