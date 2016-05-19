# define my portfolio stock choices and set up the request for data 
library(stockPortfolio)

# put together list of stocks 
stock_list_tech <- list('AAPL', 'GOOG', 'GOOGL')
stock_list_air <- list('LUV')
stock_list_retail <- list('COST')
stock_list_extra <- list('BRK-B')
stock_list_market <- list('GSPC', 'NAQ.f', '^DJI')
stock_list_full <- c(stock_list_tech, stock_list_air, stock_list_retail, 'GSPC')
# industry vector
ind <- c("tech", "tech", "tech", "other", "other")


# define some reused variables 
N <- length(stock_list_full) - 1
Rf <- 0.001 # risk free return 

# using the stockPortfolio package 
returns <- getReturns(ticker=stock_list_full, start="2008-12-31", end="2013-12-31")
# Markowitz model 
model <- stockModel(returns, Rf=Rf, drop=N+1)
#str(model)
cov_m <- model$COV

# find and plot the optimal portfolio 
OP <- optimalPort(model=model)
plot(OP, main="efficient frontier and tangent line" )
portPossCurve(model, add=TRUE)
cat("optimal portfolio assuming risk-free rate of", Rf, '\n')
print(OP$X)
cat("optimal return:", OP$R, '\n')
cat("risk at optimal return:", OP$risk, '\n')

# draw the tangent line and add a legend 
m1 <- ( OP$R - Rf ) / OP$risk
t_l <- function(x){
  m1 * x + Rf
}
plot(t_l, add=TRUE, col="green")
legend(0.08, 0.03, c("PPC","CAL"), lty=c(1,1), col=c("black","green"))

# portfolio of equally allocated funds 
x_eq <- rep(1/N, N)
R_eq <- t(x_eq) %*% colMeans(model$returns)
var_eq <- t(x_eq) %*% model$COV %*% x_eq
std_eq <- sqrt(var_eq)
print("for equally allocated funds:")
cat("mean return:", R_eq, '\n')
cat("standard deviation:", std_eq, '\n')


# Single Index Model 

# no short sales allowed 
sim_ns <- stockModel(returns, model='SIM', index=N+1, Rf=Rf, shortSelling=FALSE)

OP_sim_ns <- optimalPort(model=sim_ns)
cat("optimal portfolio of single index model, short sales NOT allowed, assuming risk-free rate of", Rf, '\n')
print(OP_sim_ns$X)
cat("optimal return:", OP_sim_ns$R, '\n')
cat("risk at optimal return:", OP_sim_ns$risk, '\n')

# short sales allowed, Rf=0 default
sim_ss <- stockModel(returns, model='SIM', index=N+1, Rf=Rf )
OP_sim_ss <- optimalPort(model=sim_ns)
cat("optimal portfolio of single index model, short sales ARE allowed, assuming risk-free rate of", Rf, '\n')
print(OP_sim_ss$X)
cat("optimal return:", OP_sim_ss$R, '\n')
cat("risk at optimal return:", OP_sim_ss$risk, '\n')



# Constant correlation model 
# short sales NOT allowed. Rf=0 again
ccm_ns <- stockModel(returns, model='CCM', drop=N+1, Rf=Rf, shortSelling=FALSE)
OP_ccm_ns <- optimalPort(model=ccm_ns)
cat("optimal portfolio of constant correlation model, short sales NOT allowed, assuming risk-free rate of", Rf, '\n')
print(OP_ccm_ns$X)
cat("optimal return:", OP_ccm_ns$R, '\n')
cat("risk at optimal return:", OP_ccm_ns$risk, '\n')


# short sales ARE allowed 
ccm_ss <- stockModel(returns, model='CCM', drop=N+1, Rf=Rf)
OP_ccm_ss <- optimalPort(model=ccm_ss)
cat("optimal portfolio of constant correlation model, short sales NOT allowed, assuming risk-free rate of", Rf, '\n')
print(OP_ccm_ss$X)
cat("optimal return:", OP_ccm_ss$R, '\n')
cat("risk at optimal return:", OP_ccm_ss$risk, '\n')




# Multi group model
# only do for short sales allowed, may not work for no short sales 
mgm <- stockModel(returns, model='MGM', drop=N+1, Rf=Rf, industry=ind)
OP_mgm <- optimalPort(model=mgm)
cat("optimal portfolio of multi group model, short sales ARE allowed, assuming risk-free rate of", Rf, '\n')
print(OP_mgm$X)
cat("optimal return:", OP_mgm$R, '\n')
cat("risk at optimal return:", OP_mgm$risk, '\n')






#str(returns)
#help(stockPortfolio)

# We can then use the round() function to round the column results to one decimal place. Or, in one step, we can create a new column that's already rounded to one decimal place:

#companiesData <- transform(companiesData, margin = round((profit/revenue) * 100, 1))

