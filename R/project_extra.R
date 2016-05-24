#avg_returns_mat <- cbind(Rs_eq, Rs_sim, Rs_half, Rs_ccm, Rs_mgm)
#avg_returns_full <- colMeans(avg_returns_mat)
#colMeans(tp_sim$returns %*% tp_sim$X)
#avg_sigmas_full <- diag(t(X_mat) %*% model$COV %*% X_mat)

# put returns and dates together into data frame for plotting
#for (i in 1:ncol(avg_returns_mat)){
#  df <- data.frame(Returns=avg_returns_mat[,i], date=date_c)
#  window_fnc()
#  plot(Returns ~ date, df, main=paste(portfolio_names[i], "portfolio"))
#}

#date_d <- date_d[-26]

#plot(tp_eq, lty=1)
#typeof(OP_sim_ns$X)
#ports <- cbind(x_eq, OP_sim_ns$X, (x_eq + OP_sim_ns$X)/2, OP_ccm_ns$X, OP_mgm$X)
#print(ret_full_matrix %*% ports)

#df <- data.frame(Returns=tp_sim$sumRet, date=date_c)
#window_fnc()
#plot(Returns ~ date, df, main=paste(portfolio_names[i], "portfolio"))

#print(dimnames(tp_eq$sumRet[1]))
#sr <- tp_eq$sumRet
#str(ret_perf)

#tp_sim$sumRet
#length(date_c)






stock_request_str1 <- "http://real-chart.finance.yahoo.com/table.csv?s="
stock_request_str2 <- "&a=11&b=31&c=2008&d=11&e=31&f=2013&g=m&ignore=.csv"
stock_list_tech <- list('AAPL', 'GOOG', 'GOOGL')
stock_list_air <- list('LUV')
stock_list_retail <- list('COST')
stock_list_extra <- list('BRK-B')
stock_list_market <- list('GSPC', 'NAQ.f', '^DJI')
stock_list_full <- c(stock_list_tech, stock_list_air, stock_list_retail, 'GSPC')

# We can then use the round() function to round the column results to one decimal place. Or, in one step, we can create a new column that's already rounded to one decimal place:
#companiesData <- transform(companiesData, margin = round((profit/revenue) * 100, 1))


get_table <- function(stock){
  rq = paste0(stock_request_str1, stock, stock_request_str2)
  a = read.table(rq, sep=',', header=TRUE)
  c = a$Adj.Close
  l = length(c)
  # negative index means exclude that index, i.e. c[-1] is list without first column 
  r = (c[-l] - c[-1]) / (c[-1])
  print("hi")
  print(c)
  print(c[-1])
  print(c[-l])
  r
}

averages <- data.frame(sapply(stock_list_market, get_table))

stock_returns <- data.frame(sapply(stock_list_full, get_table))
colnames(stock_returns) <- stock_list_full
Rbar <- colMeans(stock_returns)
cov1 <- cov(stock_returns)
icov1 <- solve(cov1)


R_1 <- Rbar - Rf
Z_1 <- icov1 %*% R_1
lambda_1 <- sum(Z_1)
x_1 <- Z_1 / lambda_1
R_G <- ( t(x_1) %*% Rbar )[1]
risk_G <- sqrt( t(x_1) %*% cov1 %*% x_1 )[1]



print(stock_data)
print(cov1)
print(Rbar)
#print(gyih[2]$Adj.Close[-64])

str(ret_hist$R)
str(OP$X)

#print(c(stock_list_full[-length(stock_list_full)]))


#str(model)
cov_m <- model$COV

#str(returns)
#help(stockPortfolio)



