# Matt Buchovecky 
# Stats M283
# Homework 4 

# the profit for buying a call if the final stock price is S, exercise price E, and cost C
P_call <- function(S, E, C){
  p <- -C
  if (S > E)
    p <- p + S - E
  p
} # end P_call
# for selling call, the profit is just the negative of this 

# profit for buying a put. to make it payout, just use C=0 
P_put <- function(S, E, C){
  p <- -C
  if (S < E)
    p <- p + E - S 
  p
} # end P_put   

new_wind <- quartz

# 1 
new_wind()
plot(Vectorize(function(S) -P_call(S, 50, 4)), from=25., to=75., col="blue", 
     xlab="stock price at expiry", ylab="profit",
     main="Profit for selling a call")
save()

# 2
new_wind()
plot(Vectorize(function(S) P_put(S, 40, 3)), from=20., to=60., col="green",
     xlab="stock price at expiry", ylab="profit",
     main="Profit for buying a put")

# 3 
new_wind()
P_3a <- function(S) 2*P_put(S, 50, 6) 
P_3b <- function(S) P_call(S, 50, 5)
P_3c <- function(S) P_3a(S) + P_3b(S)
colorvec <- c("blue", "yellow", "purple")
plot(Vectorize(P_3c), from=25., to=75., col=colorvec[3], 
     xlab="stock price at expiry", ylab="profit",
     main="Profit for buying 2 calls and 1 put")
curve(Vectorize(P_3a)(x), col=colorvec[1], add=TRUE)
curve(Vectorize(P_3b)(x), col=colorvec[2], add=TRUE)
legend(50., 30., c('calls','put','combined'), col=colorvec, lty=rep(1,3))


# 4 
new_wind()
plot(Vectorize(function(S) -2*P_call(S, 45, 5) + P_call(S, 40, 8)), from=20., to=65., col=colorvec[3], 
     xlab="stock price at expiry", ylab="profit",
     main="Profit for writing 2 calls and buying 1 call")

# 5 
new_wind()
P_bull <- Vectorize(function(S) P_call(S, 50, 0) - P_call(S, 60, 0))
P_bear <- Vectorize(function(S) P_put(S, 60, 0) - P_put(S, 50, 0))
P_box <- function(S) P_bull(S) + P_bear(S)
plot(P_box, xlim=c(35., 75.), ylim=c(0, 10.),
     col=colorvec[3], xlab="stock price at expiry", ylab="payoff",
     main="Bull, Bear, and Box spreads")
curve(P_bull, col=colorvec[1], add=TRUE)
curve(P_bear, col=colorvec[2], add=TRUE)
legend(40., 7., c('Bull','Bear','Box'), col=colorvec, lty=rep(1,3))
