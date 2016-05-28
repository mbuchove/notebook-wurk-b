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


