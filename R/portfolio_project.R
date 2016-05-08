# define my portfolio stock choices and set up the request for data 

stock_request_str1 <- "http://real-chart.finance.yahoo.com/table.csv?s="
stock_request_str2 <- "&a=11&b=31&c=2008&d=11&e=31&f=2013&g=m&ignore=.csv"
stock_list <- list('AAPL', 'LUV')
help(paste)
stk <- 'GYIH'
rqst <- paste0("http://real-chart.finance.yahoo.com/table.csv?s=",stk,"&a=11&b=31&c=2008&d=11&e=31&f=2013&g=m&ignore=.csv")
print(rqst)
