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
# now calculate by discounting the expectation of intrinsic value 
p <- (r-d)/(u-d)
pp <- (u/r)*p
k <- 6 # found from above, printed in binomial function 
j <- k:n # indices to sum over 
s <- 2*j - n
iV <- S0 * u^s - E # intrinsic value 
expVal <- sum( choose(n,j) * p^j * (1-p)^(n-j) * iV )
Cv <- expVal / r^n 
print("calculated using the discounted expectation of intrinsic value")
print(paste0("C = $", Cv))
print("they agree!")

# b, c - construct lattices 

# function to generate the binomial tree lattice, returns a column vector with node values 
gen_lattice_price <- function(S0=100, u=1.1, d=.9, N=3, E=0, option='call') {
  S <- c()
  S[1] <- S0 - E
  #ifelse
  if(S[1] < 0)
    S[1] = 0 
  count <- 2
  
  for (i in 1:N) {
    for (j in 0:i) {
      S[count] <- round(S0 * d^j * u^(i-j) - E, 2)
      if (option == 'put')
        S[count] <- -1 * S[count]
      if (S[count] < 0)
        S[count] <- 0 
      count <- count + 1
    } # for loop over nodes in time step 
  } # for loop over time steps 
  
  return(S)
} # gen_lattice

# create a dot file from the lattice, which can be used to draw a picture 
dot_lattice <- function(S, labels=FALSE) {
  shape <- ifelse(labels == TRUE, "plaintext", "point")
  
  cat("digraph G {", "\n", sep="")
  cat("node[shape=",shape,"];","\n", sep="")
  cat("rankdir=LR;","\n")
  
  cat("edge[arrowhead=none];","\n")
  
  # Create a dot node for each element in the lattice
  for (i in 1:length(S)) {
    cat("node", i, "[label=\"", S[i], "\"];", "\n", sep="")
  }
  
  # The number of levels in a binomial lattice of length N
  # is `$\frac{\sqrt{8N+1}-1}{2}$`
  L <- ((sqrt(8*length(S)+1)-1)/2 - 1)
  
  k<-1
  for (i in 1:L) {
    tabs <- rep("\t",i-1)
    j <- i
    while(j>0) {
      cat("node",k,"->","node",(k+i),";\n",sep="")
      cat("node",k,"->","node",(k+i+1),";\n",sep="")
      k <- k + 1
      j <- j - 1
    }
  } # loop over levels 
  
  cat("}", sep="")
} # dot_lattice

# 1
lat_price <- capture.output(dot_lattice(gen_lattice_price(S0=50, N=10, u=1.2, d=1./1.2), labels=TRUE))
cat(lat_price, file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/lattice_10_price.dot")
lat_iv <- capture.output(dot_lattice(gen_lattice_price(S0=50, N=10, u=1.2, d=1./1.2, E=60), labels=TRUE))
cat(lat_iv, file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/lattice_10_iv.dot")

# 2 
r <- 0.05
# adjust the rate for continous compounding 
r <- 1+exp(r/4)
u <- 1.06
d <- 0.95
p <- (r-d)/(u-d)



lat_prices <- capture.output(dot_lattice
                             (gen_lattice_price(S0=50, N=2, u=1.06, d=0.95, E=0), labels=TRUE))
cat(lat_prices, 
    file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/bpm_lattice_2_price.dot")


# 3 
lat_ivals_call <- capture.output(dot_lattice
                                 (gen_lattice_price(S0=50, N=2, u=1.06, d=0.95, E=51.00), labels=TRUE))
cat(lat_ivals_call, 
    file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/bpm_lattice_2_ivals_call.dot")

lat_ivals_put <- capture.output(dot_lattice(
  gen_lattice_price(S0=50, N=2, u=1.06, d=0.95, E=51.00, option='put'), 
  labels=TRUE))
cat(lat_ivals_put, 
    file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/bpm_lattice_2_ivals_put.dot")

# process dot files with 
# dot -Tpng -o lattice_10_nolabel.png -v lattice_10_nolabel.dot 




