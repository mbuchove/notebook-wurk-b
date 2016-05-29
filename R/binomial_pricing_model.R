# hw 5 

# 1 
binomial_formula_call <- function(S0, E, u, d, r, n){
  # calculate the probabilities 
  p <- (r-d)/(u-d)
  pp <- (u/r)*p

  # solve S0 * u^k * d^(n-k) >= E 
  k <- as.integer(ceiling(log(E/S0/d^n, u/d)))
  print("a. the number of up movements to make the call in the money:")
  print(paste("k =", k))
  
  j <- k:n # sum over every value of j that has an intrinsic value 
  Cj <- choose(n, j) * ( S0 * pp^j * (1-pp)^(n-j) - E/(r)^n * p^j * (1-p)^(n-j) )

  return(sum(Cj))
  
} # binomial_formula 

# set the values for this problem 
S0 <- 50.00 
E <- 60.00
u <- 1.2
d <- 1. / u
r <- 1.1 #r <- 0.1
n <- 10

# d 
C <- binomial_formula_call(S0=S0, E=E, u=u, d=d, r=r, n=n)
print("d. the price of the call at t=0")
print(paste0("C = $", C))
#round(C, 2)


p <- (r-d)/(u-d)
pp <- (u/r)*p

k <- 6 
j <- k:n
s <- 2*j - n
iV <- S0 * u^s - E
eVs <- choose(n,j) * p^j * (1-p)^(n-j) * iV
eV <- sum(eVs)
eV / r^n

