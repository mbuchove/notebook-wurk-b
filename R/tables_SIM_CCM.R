# HW 3 
# Exercise 1 
# read in data 
data_list <- read.table("http://www.stat.ucla.edu/~nchristo/statc183c283_10stocks.txt", header=TRUE)
#data_matrix <- as.matrix(data_list)
#print(str(data_list))

# number of stocks, one column is the date and the other is the S&P  
n <- length(data_list) - 2
t <- length(data_list[,1])

# extract just the stock prices to use to calculate returns 
stock_prices <- data_list[1:n+1] # exludes first, includes up to 11th (10th stock)
# subtract returns of t-1 from returns of t, note dates are sorted in reverse chronology
# the comma is necessary so that it doesn't access column index 
stock_returns <- ( stock_prices[-t,] - stock_prices[-1,] ) / stock_prices[-1,]
#print(str(stock_returns))
stock_means <- colMeans(stock_returns)

# calculate market mean and variance 
Rbar_m <- mean(data_list[,12])
var_m <- var(data_list[,12])


# initialize proper vectors and matrix to hold 
zeros <- rep(0, n*(n+1))
params <- matrix(zeros, nrow=n, ncol=(n+1))
stock <- rep(0, n)
alpha <- rep(0, n)
beta <- rep(0, n)
beta_var <- rep(0, n) # variance of betas 
mse <- rep(0, n) # sigma squared epsilon 
Rbar <- rep(0, n)
var_R <- rep(0, n)
sigma_R <- rep(0, n)
Ratio <- rep(0, n)
Ratio2 <- rep(0, n)

#print(str(stock_returns[,1]))
#print(str(data_list[,n+1]))

# the risk-free return:
Rf = 0.001

# now perform the regression 
for(i in 1:n){
  stock[i] <- i
  regress_m <- lm(data=stock_returns, formula=stock_returns[,i] ~ data_list[-t,n+2])
  #print(str(regress_m))
  #summary(regress_m)
  alpha[i] <- regress_m$coefficients[1]
  beta[i] <- regress_m$coefficients[2]
  beta_var[i] <- vcov(regress_m)[2,2]
  Rbar[i] <- alpha[i] + beta[i]*Rbar_m
  # divide by # d.o.f.
  mse[i] <- sum(regress_m$residuals^2) / (t-3)
  var_R[i] <- beta[i]^2 * var_m + mse[i]
  #sigma_R[i] <- sqrt(var_R[i])
  Ratio[i] <- (Rbar[i] - Rf) / beta[i]
  Ratio2[i] <- (Rbar[i] - Rf) / sqrt(var_R[i])
} # end for loop of performing regression on each stock 

# print the results 
print("Exercise 1: fit to single index model:")
print("alphas:")
print(alpha)
print("betas")
print(beta)
print("residual risks")
print(mse)
print("stock mean returns")
print(Rbar)
print("stock mean returns calculated simply for comparison")
print(stock_means)
print("stock variances")
print(var_R)
print("mean market return")
print(Rbar_m)
print("variance of market")
print(var_m)

beta_mean <- mean(beta)
var_mean_beta <- var(beta)

# now apply Vasicek's technique to forecast adjust the betas 
beta_2 <- ( beta_var ) / (var_mean_beta + beta_var) * beta_mean + ( var_mean_beta ) / (var_mean_beta + beta_var) * beta
print("betas adjusted by Vasicek's technique")
print(beta_2)


# Exercise 2 
# construct the optimal portfolio using the single index model 
table <- cbind(stock, alpha, beta, Rbar, mse, Ratio)
ord_tab <- table[order(-Ratio), ]

# k = (Rbar - Rf) * beta / mse # but needs to be ordered 
k <- (ord_tab[,4]-Rf)*ord_tab[,3] / ord_tab[,5] 
sum_k <- cumsum(k)
l <- ord_tab[,3]^2 / ord_tab[,5] # l = beta^2 / mse
sum_l <- cumsum(l)

# find C, it will be ordered properly 
C <- var_m * sum_k / (1 + var_m*sum_l)
ord_tab <- cbind(ord_tab, k, sum_k, l, sum_l, C)
print("table for single index model:")
print(ord_tab)

C_ss <- C[n] # short sales are allowed 
cat("if short sales are allowed, C* =", C_ss, '\n')
i = 1 
while(i <= n && C[i] < ord_tab[i,6]){
  i <- i + 1
} # iterate through until you meet condition for C*, then step back 1
i <- i - 1 
C_ns <- C[i]
cat("if short sales are not allowed, C* =", C_ns, '\n')
if(i == which(C==max(C))){
  print("C* for no short sales is the max value of C")
}

# find composition of optimal portfolio if short sales are allowed 
z <- (beta/mse) * ( (Rbar-Rf)/beta - C_ss)
x_ss <- z / sum(z)
print("composition of optimal portfolio if short sales are allowed: (stocks in order of original index)")
print(x_ss)

# find composition of optimal portfolio if short sales are not allowed 
ord_tab_ns <- ord_tab[1:i,]
z_ns <- (ord_tab_ns[,3]/ord_tab_ns[,5]) * ( (ord_tab_ns[,4]-Rf)/ord_tab_ns[,3] - C_ns)
x_ns <- z_ns / sum(z_ns)
print("composition of optimal portfolio if short sales are not allowed:")
print(cbind(ord_tab_ns[,1], x_ns))


# Exercise 3 
# calculate the average correlation 
cov_stocks <- cov(stock_returns)

rho <- 0
for(i in 1:n){
  for(j in 1:n){
    if(i != j){
      rho <- rho + cov_stocks[i,j] / sqrt(cov_stocks[i,i]*cov_stocks[j,j])
    }
  }
} # sum over all non-diagonal correlations 
rho <- rho / ( n*(n-1) )
cat("average of rho:", rho, '\n')

s_i <- 1:n
sigma_R <- sqrt(var_R)

tab_ccm <- cbind(stock, Rbar, sigma_R, Ratio2)[order(-Ratio2) ,]
sum_R2 <- cumsum(tab_ccm[,4]) # cumulative sum of ordered ration 
ratio_rho <- rho / (1+rho*(s_i-1))
C_ccm <- ratio_rho * sum_R2
tab_ccm <- cbind(tab_ccm, ratio_rho, sum_R2, C_ccm)
print("table for constant correlation model:")
print(tab_ccm)

#print("constant correlation model")

C_ccm_ss <- C_ccm[n] # short sales are allowed 
cat("if short sales are allowed, C* =", C_ccm_ss, '\n')
i = 1 
while(i <= n && C_ccm[i] < tab_ccm[i,4]){
  i <- i + 1
} # iterate through until you meet condition for C* 
i <- i - 1
C_ccm_ns <- C_ccm[i]
cat("if short sales are not allowed, C* =", C_ccm_ns, '\n')
if(i == which(C_ccm==max(C_ccm))){
  print("C* for no short sales is the max value of C")
}

# find composition of optimal portfolio if short sales are allowed 
#z_ccm_ss <- 1/((1-rho)*sigma_R) * ( (Rbar-Rf)/ - C_ccm_ss)
z_ccm_ss <- (tab_ccm[,4]-C_ccm_ss)/((1-rho)*tab_ccm[,3])      
x_ccm_ss <- z_ccm_ss / sum(z_ccm_ss)
print("composition of optimal portfolio if short sales are allowed:")
print(cbind(tab_ccm[,1], x_ccm_ss))

# find composition of optimal portfolio if short sales are not allowed 
tab_ccm_ns <- tab_ccm[1:i,] # only go up to index of last positive stock 
z_ccm_ns <- (tab_ccm_ns[,4]-C_ccm_ns)/((1-rho)*tab_ccm_ns[,3])
x_ccm_ns <- z_ccm_ns / sum(z_ccm_ns)
print("composition of optimal portfolio if short sales are not allowed:")
print(cbind(tab_ccm_ns[,1], x_ccm_ns))

#system.time(case1)
#cat("\014") 
