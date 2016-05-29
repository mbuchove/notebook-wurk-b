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
                             (gen_lattice_price(S0=50, N=3, u=1.06, d=0.95, E=0), labels=TRUE))
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



