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
x_A <- x_G
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
cov_AB = (t(x_A) %*% cov_145 %*% x_B)[1]
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
cov_t <- rho*sig_A*sig_B
# a. 
R_B <- ( R_A*(sig_B^2 + cov_t) + R_f*(sig_A^2 - sig_B^2) ) / (sig_A^2 + cov_t)
print("Exercise 2, return of B:")
print("for each stock even")
print(R_B)
# b. 
R_B <- R_f + (R_A - R_f)*cov_t / sig_A^2
print("for no stock B")
print(R_B) 

# Exercise 3 
# a. 
print("Exercise 3:")
rho <- (0.0525 - 0.25*0.16 - 0.25*0.25) / (2*0.25*0.4*0.5)
print(rho*0.4*0.5)
# b. 
R_p <- 0.11
R_f <- 0.05
R_G <- ( t(c(0.6, 0.4)) %*% c(0.14, 0.1) )[1]
x_G <- (R_p - R_f) / (R_G - R_f)
print(1-x_G)
# c. 
R_p <- 0.10
x_G <- (R_p - R_f) / (R_G - R_f)
print(1-x_G)


# Exercise 5 
# a. 
ones <- rep(1, 2)
iQ <- matrix(c(166.21139, -22.40241, -22.40241, 220.41076), nrow=2, ncol=2)
Q <- solve(iQ)
x_min <- iQ %*% ones / as.numeric(t(ones) %*% iQ %*% ones)
print("Exercise 5; minimum risk portfolio composition:")
print(x_min)
# b. 
R_f <- 0.011
sd_A <- sqrt((t(x_min) %*% Q %*% x_min)[1])
R_A <- 0.01315856
R_B <- 0.01219724
# x_f * R_f + (1-x_f) * R_A = R_B
x_f <- ( R_A - R_B ) / (R_A - R_f)
print("composition of portfolio for A and risk free, respectively:")
print(1-x_f)
print(x_f)



# Exercise 6 
# single index variance
beta_A = 0.79
beta_B = 1.12
var_eps_A = 0.027
var_eps_B = 0.006
var_m = 0.0022
var_si <- function(beta, var_eps, var_m) {
  beta^2*var_m + var_eps
}
var_A <- var_si(beta_A, var_eps_A, var_m)
var_B <- var_si(beta_B, var_eps_B, var_m)
rho6 <- beta_A*beta_B*var_m/(sqrt(var_A)*sqrt(var_B))
print("Exercise 6: correlation coefficient:")
print(rho6)
# c. 
print(sqrt(0.006/0.13))


# Exercise 7 
# a. 
Rbar_m7 <- 0.10
var_m7 <- 0.002
x_7 <- c(0.3, 0.5, 0.2)
alphas_7 <- c(0.01, 0.04, 0.08)
betas_7 <- c(1.08, 0.80, 1.22)
sigma_eps_7 <- c(0.003, 0.006, 0.001)
beta_p7 <- t(x_7) %*% betas_7
print("Exercise 7, beta of optimum portfolio:")
Rbar_opt7 <- ( t(x_7) %*% alphas_7 + t(x_7) %*% betas_7 * Rbar_m7 )[1]
print("expected return of optimum portfolio")
print(Rbar_p7)

# b. 
beta_matrix_7 <- betas_7 %*% t(betas_7)
print(beta_matrix_7)
cov_matrix_7 <- beta_matrix_7*var_m7 + diag(sigma_eps_7)
print("standard deviation of optimum portfolio")
risk_opt7 <- sqrt( t(x_7) %*% cov_matrix_7 %*% x_7 )[1]
print(risk_opt7)
R_f7 <- 0.002
dollars_p7 <- c(800000, -300000)
x_p7 <- dollars_p7 / sum(dollars_p7)
R_7 <- c(Rbar_opt7, R_f7)
R_p7 <- t(x_p7) %*% R_7
print("expected return of portfolio with borrowing:")
print(R_p7)
risk_p7 <- ( x_p7[1]*risk_opt7 )[1]
print("expected standard deviation of portfolio with borrowing:")
print(risk_p7)

# c. 
cov_pm <- ( t(x_7) %*% betas_7 )[1] * var_m7
print("covariance of optimum portfolio and the market")
print(cov_pm)

# d. 
x_d7 <- c(0.6, 0.4)
R_d7 <- ( t(x_d7) %*% R_7 )[1]
risk_d7 <- x_d7[1]*risk_opt7
print("risk and return for 7d.")
print(risk_d7)
print(R_d7)

print(1.08*.002)
