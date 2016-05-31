# HW 6 

# 1
# distribution of stock during single time interval 
# assume it follows a Wiener process 

S0 <- 50.00 # initial price of stock 
mu <- 0.20 # expected return 
sig <- 0.25 # standard deviation of price 
n <- 52
dt <- 1. / n # delta t is one week 

mean <- mu*dt
sd <- sig*sqrt(dt)
x <- seq(mean-3*sd, mean+3*sd, length=100)
R <- dnorm(x, mean=mean, sd=sd) 
plot(x, R, xlab="return", ylab="density", main="return of stock during 1 week")
dS <- rnorm(n, mean=mean, sd=sd)
S_t <- S0 + cumsum(dS)
t <- 1:n

print(S_t)
plot(t, S_t, xlab="t(week)", ylab="stock price", main="simulation of stock price over time")


# 2 
S2 <- 50.00
mu2 <- 0.16
sig2 <- 0.30
t2 <- 1/365 # T-t 
expected_price <- function(t, mu=0.1, S=100.00){
  return( S*exp(mu*t) )
} # calculated the expected price of stock in future 
stdev_price <- function(t, sig=0.5, mu=0.1, S=100.00){
  return( S*exp(mu*t)*sqrt(exp(sig^2*t)-1) )
} # calculate standard deviation of price of stock in future 
EST <- expected_price(t2, mu2, S2)
SST <- stdev_price(t2, sig2, mu2, S2)
print(paste("the expected stock price at end of next day:", EST))
print(paste("the standard deviation of next day stock price:", SST))
#EST <- S2+mu2*t2
#VST <- sig2*sqrt(t2)


# 3 
S3 <- 38.00
mu3 <- 0.16
sig3 <- 0.35 
t3 <- 1/2
E3 <- 40.00
#use pnorm(s, mean, sd) to find the probability the stock will fall below the exercise price
p_put <- pnorm(E3, expected_price(t3, mu3, S3), stdev_price(t3, sig3, mu3, S3))
p_call <- 1 - p_put
print(paste0("there is a ", round(p_call*100, 5), "% chance that the call will be exercised"))
print(paste0("and there is a ", round(p_put*100, 5), "% chance that the put will be exercised"))


# 4 
mu4 <- 0.10
sig4 <- 0.15 
S4 <- 40.00
t4 <- 0. 
T4 <- 2/12 
# a 
term1 <- (mu4-sig4^2/2)*(T4-t4)
term2 <- 1.96*sig4*sqrt(T4-t4)
Slow <- S4 * exp(term1 - term2)
Shigh <- S4 * exp(term1 + term2)
print("the 95% confidence interval for the stock in 2 months:")
print(paste(round(Slow,5), "< S(T) <", round(Shigh,5)))

# b 
print(paste("the expected price of the stock in 2 months is", expected_price(T4, mu4, S4)))
# c
print(paste("the expected standard deviation of the stock price in 2 months is", stdev_price(T4, sig4, mu4, S4)))


# 5 
S5 <- 95.00
sig5 <- 0.6
E5 <- 105.00 
r5 <- 0.08 
t5 <- 8/12

d1 <- (log(S5/E5) + (r+sig5^2/2)*t5) / (sig5*sqrt(t5))
# d2 = (ln(S0/E) + (r-1/2*sig^2)*t) / (sig*sqrt(t))
d2 <- d1 - sig5*sqrt(t5)
# Phi is the cumulative distribution function of the standard normal distribution 
C5 <- S5*pnorm(d1) - E5/exp(r5*t5)*pnorm(d2)
print(paste0("the value of the call calculated with the Black-Scholes model is $", round(C5,5)))


# 6 
sA <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=01&b=01&c=2016&d=04&e=27&f=2016&g=d&ignore=.csv", 
               sep=",", header=TRUE)
rA <- (sA$Adj.Close[-length(sA)] - sA$Adj.Close[-1]) / (sA$Adj.Close[-1])
rA
(sA$Adj.Close[-length(sA)] - sA$Adj.Close[-1])
#r1 <- (a$P1[-data_len] - a$P1[-1]) / (a$P1[-1])
sA
sA$Adj.Close
sA$Adj.Close[-length(sA)]
sA$Adj.Close[-1]

