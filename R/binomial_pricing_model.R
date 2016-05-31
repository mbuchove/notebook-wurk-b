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
  
} # binomial_formula_price 

# set the values for this problem 
S0 <- 50.00 
E1 <- 60.00
u1 <- 1.2
d1 <- 1. / u1
r1 <- 1.1 #r <- 0.1
n1 <- 10

# d 
C <- binomial_formula_call(S0=S0, E=E1, u=u1, d=d1, r=r1, n=n1)
print("d. the price of the call at t=0")
print(paste0("C = $", C))

# now calculate by discounting the expectation of intrinsic value 
p1 <- (r1-d1)/(u1-d1)
p1p <- (u1/r1)*p1
k <- 6 # found from above, printed in binomial function 
j <- k:n1 # indices to sum over 
s <- 2*j - n1
iV <- S0 * u1^s - E1 # intrinsic value 
expVal <- sum( choose(n1,j) * p1^j * (1-p1)^(n1-j) * iV )
Cv <- expVal / r1^n1
print("calculated using the discounted expectation of intrinsic value")
print(paste0("C = $", Cv))
print("they agree!")

# b, c - construct lattices 

# function to generate the binomial tree lattice, returns a column vector with node values 
gen_lattice_price <- function(S0=100.00, u=1.1, d=1./1.1, N=3, E=0.00, option='call') {
  S <- c()
  ifelse(option=='call', S[1]<-S0-E, S[1]<-E-S0)
  if(S[1] < 0) # if stock price falls below exercise price, call is worthless
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
  cat("node[shape=",shape,"];" , "\n", sep="")
  cat("rankdir=LR;", "\n")
  cat("edge[arrowhead=none];", "\n")
  
  # Create a dot node for each element in the lattice
  for (i in 1:length(S)) {
    cat("node", i, "[label=\"", S[i], "\"];", "\n", sep="")
  }
  
  # The number of levels in a binomial lattice of length N
  # solve length(S) = n*(n-1)/2
  L <- ((sqrt(8*length(S)+1)-1)/2 - 1)
  
  k<-1
  for (i in 1:L) {
    j <- i
    while(j>0) {
      cat("node",k,"->","node",(k+i),";\n",sep="")
      cat("node",k,"->","node",(k+i+1),";\n",sep="")
      k <- k + 1
      j <- j - 1
    } # loop over nodes in level 
  } # loop over levels 
  
  cat("}", sep="")
} # dot_lattice

# 1
lat_price <- capture.output(dot_lattice(gen_lattice_price(S0=S0, N=n1, u=u1, d=d1), labels=TRUE))
cat(lat_price, file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/bpm_lattice_10_price.dot")
lat_iv <- capture.output(dot_lattice(gen_lattice_price(S0=S0, N=n1, u=u1, d=d1, E=E1), labels=TRUE))
cat(lat_iv, file="/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/bpm_lattice_10_iv.dot")

# process dot files with 
# dot -Tpng -o bpm_lattice_10_prices.png -v bpm_lattice_10_prices.dot 
dot_dir <- "/Users/mbuchove/Dropbox/Physics/ProbabilityStatistics/Stats_C283/"

# function to take the intrinsic values lattice and step backward, creating call prices 
option_prices <- function(lat, p){ # input lattice, intrinsic values (gen_lattice_price with E specified)
  n <- length(lat)
  L <- ((sqrt(8*n+1)-1)/2 - 1) # the number of levels in a binomial tree 
  ls <- lat[(n-L):n] # the last column of intrinsic values 
  
  ln <- c() # start the new lattice to return 
  
  level_call <- function(l){
    lt <- c()
    for(i in 1:(length(l)-1) )
      lt[i] <- p*l[i] + (1-p)*l[i+1]
    ln <<- c(ln, rev(lt))
    
    if(length(lt) > 1)
      level_call(lt)
    
    return(rev(ln))
  }
  
  return(level_call(ls))
  
} # call price 


# 2 / 3 
r1 <- 0.05
# adjust the rate for continous compounding 
r <- exp(r1/4)
u2 <- 1.06
d2 <- 0.95
p2 <- (r-d2)/(u2-d2)
E2 <- 51.00
n2 <- 2

lat_prices <- gen_lattice_price(S0=S0, N=n2, u=u2, d=d2, E=0.00)
dot_prices <- capture.output(dot_lattice(lat_prices, labels=TRUE))
cat(dot_prices, file=paste0(dot_dir, "bpm_lattice_2_price.dot") )


# 2, call 
lat_ivals_call <- gen_lattice_price(S0=S0, N=n2, u=u2, d=d2, E=E2)
dot_ivals_call <- capture.output(dot_lattice(lat_ivals_call, labels=TRUE))
cat(dot_ivals_call, file=paste0(dot_dir, "bpm_lattice_2_ivals_call.dot") ) 

lat_callprices <- option_prices(lat_ivals_call, p2 )
dot_callprices <- capture.output(dot_lattice(lat_callprices, labels=TRUE))
cat(dot_callprices, file=paste0(dot_dir, "bpm_lattice_2_callprices.dot") )

C0 <- lat_callprices[1]
print(paste0("the value of the 6 month call is $", C0))


# 3, put 
lat_ivals_put <- gen_lattice_price(S0=S0, N=n2, u=u2, d=d2, E=E2, option='put')
dot_ivals_put <- capture.output(dot_lattice(lat_ivals_put, labels=TRUE))
cat(dot_ivals_put, file=paste0(dot_dir, "bpm_lattice_2_ivals_put.dot") )

lat_putprices <- option_prices(lat_ivals_put, p2)
dot_putprices <- capture.output(dot_lattice(lat_putprices, labels=TRUE))
cat(dot_putprices, file=paste0(dot_dir, "bpm_lattice_2_putprices.dot") )

P0 <- lat_putprices[1]
print(paste0("the value of the 6 month put is $", P0))

print("check for put-call parity:")
print(paste("C + E/e^(2rt) =", C0 + E2/exp(2*r1*1/4) ))
print(paste("P + S0 =", P0 + S0 ))

# 4 - American option
#American_option <- function(call_vals, intrinsic_vals){}
#Anode[i] <- max(call_vals[i], intrinsic_vals[i])
Pamerican <- p2 * max(lat_putprices[2], lat_ivals_put[2]) + (1-p2) * max(lat_putprices[3], lat_ivals_put[3])
cat("the value of the American put option is $", Pamerican, sep='')
cat("it is optimal to exercise early if the first step is a down step")
