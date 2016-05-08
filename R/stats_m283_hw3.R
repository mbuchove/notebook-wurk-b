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
Ratio <- rep(0, n)

print(str(stock_returns[,1]))
print(str(data_list[,n+1]))

# the risk-free return:
Rf = 0.001

# now perform the regression 
for(i in 1:n){
  stock[i] <- i
  regress_m <- lm(data=stock_returns,formula=stock_returns[,i] ~ data_list[-t,n+2])
  #print(str(regress_m))
  #summary(regress_m)
  alpha[i] <- regress_m$coefficients[1]
  beta[i] <- regress_m$coefficients[2]
  beta_var[i] <- vcov(regress_m)[2,2]
  Rbar[i] <- alpha[i] + beta[i]*Rbar_m
  # divide by # d.o.f.
  mse[i] <- sum(regress_m$residuals^2) / (t-3)
  var_R[i] <- beta[i]^2 * var_m + mse[i]
  Ratio[i] <- (Rbar[i] - Rf) / beta[i]
}

# print the results 
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

table <- cbind(stock, alpha, beta, Rbar, mse, Ratio)
ord_tab <- table[order(-Ratio)]

k <- (Rbar-Rf)*beta / mse
print(str(k))









