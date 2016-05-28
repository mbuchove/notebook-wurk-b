# hw 5 

binomial_formula_call <- function(S0, E, u, d, r, n){
  p <- (r-d)/(u-d)
  pp <- (u/r)*p
  print(p)
  print(pp)
  
  k <- ceiling( log(E/S0, base=u) )
  if (u^k == E/S0)
    k <- k+1
  k <- as.integer(ceiling( (n+k)/2) )
  
  j <- k:n
  Cj <- choose(n, j) * ( S0 * pp^j * (1-pp)^(n-j) - E/(r)^n * p^j * (1-p)^(n-j) )
  
  sum(Cj)
  
} # binomial_formula 

# 1 

S0 <- 50.00 
E <- 60.00
u <- 1.2
d <- 1. / u
r <- 1.1
#r <- 0.1
n <- 10

C <- binomial_formula_call(S0=S0, E=E, u=u, d=d, r=r, n=n)
print(C)

p <- (r-d)/(u-d)
pp <- (u/r)*p

k <- 6 
j <- k:n
s <- 2*j - n
iV <- S0 * u^s - E
eVs <- choose(n,j) * p^j * (1-p)^(n-j) * iV
eV <- sum(eVs)
eV / r^n

