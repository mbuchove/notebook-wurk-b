# define my portfolio stock choices and set up the request for data 
library(stockPortfolio)

# put together list of stocks 
stock_list_tech <- list('GOOG', 'GOOGL', 'ADBE', 'MSFT', 'TXN') # technology 
stock_list_goods <- list('AAPL', 'NKE', 'HMC', 'MERC', 'VOXX') # consumer goods 
stock_list_health <- list('CLC', 'SUNPHARMA.BO', 'SHP.L', 'PFE', 'FORTIS.BO') # healthcare 
stock_list_svc <- list('COST', 'LUV', 'AXFO.ST', 'HIBB', 'DSW') # services 
stock_list_fin <- list('BRK-B', 'ITG', 'BANC', 'KRNY', 'JLL') # financial 
stock_list_mkt <- list('GSPC') # market , 'NAQ.f', '^DJI'
stock_list_full <- c(stock_list_tech, stock_list_goods, stock_list_health, stock_list_svc, stock_list_fin,  stock_list_mkt)
# industry vector
#ind <- c()
sectors <- c("tech", "tech", "tech", "tech", "tech", 
             "goods", "goods", "goods", "goods", "goods", 
             "health", "health", "health", "health", "health", 
             "service", "service", "service", "service", "service", 
             "financial", "financial", "financial", "financial", "financial")

# define some reused variables 
N <- length(stock_list_full) - 1
Rf <- 0.001 # risk free return 


# using the stockPortfolio package 
ret_hist <- getReturns(ticker=stock_list_full, start="2008-12-31", end="2013-12-31")
print(ret_hist$R)

# Markowitz model 
model <- stockModel(ret_hist, Rf=Rf, drop=N+1)

# determine which function to use to create new windows for separate plots 
kernel_name <- Sys.info()["sysname"]
if (kernel_name == "Darwin"){
  window_fnc <- quartz 
} else if (kernel_name == "Linux"){
  window_fnc <- x11 
} else if (kernel_name == "Windows"){
  window_fnc <- windows() 
} else{
  window_fnc <- X11 
}
#dev.new()
window_fnc()


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
Rbar <- colMeans(model$returns)
x_eq <- rep(1/N, N)
R_eq <- t(x_eq) %*% Rbar
var_eq <- t(x_eq) %*% model$COV %*% x_eq
std_eq <- sqrt(var_eq)
print("for equally allocated funds:")
cat("mean return:", R_eq, '\n')
cat("standard deviation:", std_eq, '\n')


# Single Index Model 

# no short sales allowed 
sim_ns <- stockModel(ret_hist, model='SIM', index=N+1, Rf=Rf, shortSelling=FALSE)

OP_sim_ns <- optimalPort(model=sim_ns)
cat("optimal portfolio of single index model, short sales NOT allowed, assuming risk-free rate of", Rf, '\n')
print(OP_sim_ns$X)
cat("optimal return:", OP_sim_ns$R, '\n')
cat("risk at optimal return:", OP_sim_ns$risk, '\n')
print("the alphas are:")
print(sim_ns$alpha)
print("the betas are:")
print(sim_ns$beta)

# short sales allowed, Rf=0 default
sim_ss <- stockModel(ret_hist, model='SIM', index=N+1, Rf=Rf )
OP_sim_ss <- optimalPort(model=sim_ss)
cat("optimal portfolio of single index model, short sales ARE allowed, assuming risk-free rate of", Rf, '\n')
print(OP_sim_ss$X)
cat("optimal return:", OP_sim_ss$R, '\n')
cat("risk at optimal return:", OP_sim_ss$risk, '\n')
print("the alphas are:")
print(sim_ss$alpha)
print("the betas are:")
print(sim_ss$beta)
print("the alphas and betas do not depend on whether or not short selling is allowed")

# Constant correlation model 
# short sales NOT allowed. Rf=0 again
ccm_ns <- stockModel(ret_hist, model='CCM', drop=N+1, Rf=Rf, shortSelling=FALSE)
OP_ccm_ns <- optimalPort(model=ccm_ns)
cat("optimal portfolio of constant correlation model, short sales NOT allowed, assuming risk-free rate of", Rf, '\n')
print(OP_ccm_ns$X)
cat("optimal return:", OP_ccm_ns$R, '\n')
cat("risk at optimal return:", OP_ccm_ns$risk, '\n')


# short sales ARE allowed 
ccm_ss <- stockModel(ret_hist, model='CCM', drop=N+1, Rf=Rf)
OP_ccm_ss <- optimalPort(model=ccm_ss)
cat("optimal portfolio of constant correlation model, short sales ARE allowed, assuming risk-free rate of", Rf, '\n')
print(OP_ccm_ss$X)
cat("optimal return:", OP_ccm_ss$R, '\n')
cat("risk at optimal return:", OP_ccm_ss$risk, '\n')


# Multi group model
# only do for short sales allowed, may not work for no short sales 
mgm <- stockModel(ret_hist, model='MGM', drop=N+1, Rf=Rf, industry=sectors)
OP_mgm <- optimalPort(model=mgm)
cat("optimal portfolio of multi group model, short sales ARE allowed, assuming risk-free rate of", Rf, '\n')
print(OP_mgm$X)
cat("optimal return:", OP_mgm$R, '\n')
cat("risk at optimal return:", OP_mgm$risk, '\n')

# part 7, plot all stocks 
points(model$sigma, Rbar, col='blue')
# points of optimal portfolios
opt_risks <- c(OP$risk, OP_sim_ns$risk, OP_sim_ss$risk, OP_ccm_ns$risk, OP_ccm_ss$risk, OP_mgm$risk)
opt_Rs <- c(OP$R, OP_sim_ns$R, OP_sim_ss$R, OP_ccm_ns$R, OP_ccm_ss$R, OP_mgm$R)
opt_risks
points(opt_risks, opt_Rs, col='yellow', pch=18)
#savePlot()


# Part B
# for simplicity, create a matrix of all the X's for each portfolio 
ret_hist_matrix <- as.matrix(ret_hist$R[,-(N+1)])
x_sim <- OP_sim_ns$X
x_half <-  (x_eq + OP_sim_ns$X)/2
x_ccm <- OP_ccm_ns$X
x_mgm <- OP_mgm$X
X_mat <- cbind(x_eq, x_sim, x_half, x_ccm, x_mgm)

# calculate the monthly returns for each portfolio with historical data 
Rs_hist <- ret_hist_matrix %*% X_mat
print("average monthly returns for each portfolio from historical data")
print(colMeans(Rs_hist))


# get returns in more recent date range to test performance of portfolios 
ret_perf <- getReturns(ticker=stock_list_full, start="2013/12/31", end="2016/04/30")
ret_perf2 <- getReturns(ticker=stock_list_full[-(N+1)], start="2013/12/31", end="2016/04/30")
ret_mkt_perf <- ret_perf$R[,N+1]
ret_perf_matrix <- as.matrix(ret_perf2$R)
date_strings <- dimnames(ret_perf_matrix)[[1]]
date_c <- as.Date(x=date_strings, format="%Y-%m-%d" )

# now find the monthly returns for newer data
Rs_perf <- ret_perf_matrix %*% X_mat
avg_ret_perf <- colMeans(Rs_perf)
print("average monthly returns for each portfolio from recent data")
print(colMeans(Rs_perf))
#print("cumulative returns for each portfolio")

options(warn=-1)
# test portfolio command from stockPortfolio package to get cumulative returns 
tp_eq <- testPort(theData=ret_perf2, X=x_eq)
tp_sim <- testPort(ret_perf, OP_sim_ns)
tp_half <- testPort(ret_perf, X=x_half)
tp_ccm <- testPort(ret_perf, OP_ccm_ns)
tp_mgm <- testPort(ret_perf, OP_mgm)
options(warn=1)
print(tp_eq$sumRet)

# plot the cumulative returns together with the market average 
window_fnc()
models <- c('Equal', 'SIM', 'Half', 'CCM', 'MGM', 'S&P')
ltypes <- 1:6
colors <- c("black", "red", "green", "blue", "cyan", "pink")
plot(tp_mgm, lty=5, xlab="month", main="cumulative returns", col='cyan')
lines(tp_eq, lty=1)
lines(tp_sim, col='red')
lines(tp_half, col='green')
lines(tp_ccm, col='blue')
lines(cumprod(1+rev(ret_mkt_perf)), col="purple", lwd=2)
legend(0.01, 1.8, models, ltypes, col=colors)

# combine all the returns, and calculate total returns for all portfolios in full date range
ret_full_matrix <- rbind(ret_perf_matrix, ret_hist_matrix)
Rs_full <- ret_full_matrix %*% X_mat

# create column vector of dates using Date object 
portfolio_names <- c("equally allocated", "optimal SIM", "half eq half SIM", "optimal CCM", "optimal MGM")
date_strings <- dimnames(ret_perf_matrix)[[1]]
date_c <- as.Date(x=date_strings, format="%Y-%m-%d" )
# all average returns combined into a matrix 

# sharpe ratio - done for historical data 
cov_perf <- cov(ret_perf$R[,-(N+1)])
sig_port <- diag( t(X_mat) %*% cov_perf %*% X_mat ) 
sharpe_ratio <- (avg_ret_perf - Rf) / sig_port 
names(sharpe_ratio) <- portfolio_names
print("Sharpe ratios:")
print(sharpe_ratio)


# Treynor ratio, first calculate betas for new data 
sim_perf <- stockModel(ret_perf, model='SIM', index=N+1, Rf=Rf )
beta_mat <- as.matrix(sim_perf$beta)
avg_betas <- t(X_mat) %*% beta_mat
treynor_measure <- ( (avg_ret_perf - Rf) / avg_betas )[,1]
names(treynor_measure) <- portfolio_names
print("Treynor measures: ")
print(treynor_measure)

# Jensen differential performance index 
mkt_avg <- mean(ret_mkt_perf)
jensen_measure <- ( avg_ret_perf - (Rf + avg_betas * (mkt_avg - Rf)) )[,1]
names(jensen_measure) <- portfolio_names
print("Jensen measure: ")
print(jensen_measure)

# differential return with risk measured by standard deviation 
sig_mkt <- sqrt(var(ret_mkt_perf))
diff_ret <- avg_ret_perf - ( Rf + (mkt_avg-Rf) * sig_port/sig_mkt )
names(diff_ret) <- portfolio_names
print("differential returns with risk measured by standard deviation")
print(diff_ret)
