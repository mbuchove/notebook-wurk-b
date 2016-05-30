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
EST <- S2*exp(mu2*t2)
SST <- S2*exp(mu2*t2)*sqrt(exp(sig2^2*t2)-1)
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



# 4 
mu4 <- 0.10
sig4 <- 0.15 
S4 <- 40.00
t4 <- 0. 
T4 <- 2/12 
term1 <- (mu4-sig4^2/2)*(T4-t4)
term2 <- 1.96*sig4*sqrt(T4-t4)
Slow <- S4 * exp(term1 - term2)
Shigh <- S4 * exp(term1 + term2)
print("the 95% confidence interval for the stock in 2 months:")
print(paste(round(Slow,5), "< S(T) <", round(Shigh,5)))


# 5 
S5 <- 95.00
sig5 <- 0.6
E5 <- 105.00 
r5 <- 0.08 
t5 <- 8/12



# 6 
s1 <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=01&b=01&c=2016&d=04&e=27&f=2016&g=d&ignore=.csv", 
               sep=",", header=TRUE)
