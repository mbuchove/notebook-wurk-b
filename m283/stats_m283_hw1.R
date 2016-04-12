# Exercise 1 and 2 

# initialize variables 
R1_bar <- 0.01
R2_bar <- 0.013
var1 = 0.0061
var2 = 0.0046
sd1 = sqrt(var1)
sd2 = sqrt(var2)

# define functional form of ro wand optimize
rho_f <- function(x1) {(var2-x1^2*var1-(1-x1)^2*var2)/(2*x1*(1-x1)*sd1*sd2)}
curve(rho_f, from=0, to=1, xname="x1") # yname is not a graphical parameter 
print(optimize(rho_f, interval=c(0,1), maximum=TRUE))
# the exact answer from solving analytically, agrees with minimizer 
rho_min <- sqrt(var2/var1)
print(rho_min)

# create sequence of x1 and x2 values following constraint for no short sales
x1_seq <- seq(-1, 1, 0.01)
x2_seq <- 1 - x1_seq

# create the points for return and risk for the portfolio 
Rp_bar <- x1_seq*R1_bar + x2_seq*R2_bar
var_p <- x1_seq^2*var1 + x2_seq^2*var2 + 2*x1_seq*x2_seq*rho_min*sd1*sd2
sd_p <- sqrt(var_p)

# plot 
plot(sd_p, Rp_bar, col="blue", xlab="Stdev of portfolio", ylab="expected return of portfolio")


# Exercise 4 
sig2_avg = 50 
cov_avg = 10 
sig2_p <- function(n) {sig2_avg/n+(n-1)*cov_avg/n}
n_vec <- c(5, 10, 20, 50, 100)
risk_vec = sig2_p(n_vec)
print(risk_vec)
