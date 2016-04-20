# Exercise 1 

# read the table 
a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c183_c283/statc183c283_5stocks.txt", header=T)
data_len = length(a$date)

# find returns:
# Exxon-Mobil
r1 <- (a$P1[-data_len] - a$P1[-1]) / (a$P1[-1])
# General Motors
r2 <- (a$P2[-data_len] - a$P2[-1]) / (a$P2[-1])
# Hewlett Packard
r3 <- (a$P3[-data_len] - a$P3[-1]) / (a$P3[-1])
# McDonalds
r4 <- (a$P4[-data_len] - a$P4[-1]) / (a$P4[-1])
# Boeing
r5 <- (a$P5[-data_len] - a$P5[-1]) / (a$P5[-1])

# create data frame using the returns, then find means and covariance, then print
rr <- data.frame(r1, r2, r3, r4, r5)
means <- colMeans(rr)
cov.mat <- cov(rr)
print(means)
print(cov.matrix)

# c. make sub-matrix and vectors for just these 3 stocks 
indices <- c(1, 4, 5)
r145 = means[indices]
cov145 = cov.mat[indices, indices]

# iterate over different combinations of stocks and append to a data frame  
xs <- read.table("http://www.stat.ucla.edu/~nchristo/datac183c283/statc183c283_abc.txt", header=T)
num_points = length(xs$"a")
df = data.frame(Eret=double(num_points), SD=double(num_points))
i <- 1
r_s <- apply(xs, 1, function(row) {
  re <- (t(row) %*% r145)[1][1]
  sd <- (t(row) %*% cov145 %*% row)[1][1]
  print(re[1][1])
  df$Eret[i] <<- re
  df$SD[i] <<- sd
  i <<- i + 1 # double arrow makes it accessible outside of function 
})

print(min(df$Eret))
print(max(df$Eret))
# now plot the points 
plot(-0.002595035, 0.0155, main="Portfolio Possibilities Curve", 
     xlab="standard deviation", 
     ylab="Expected Return", 
     type ="n", 
     xlim=c(min(df$SD), max(df$SD)), 
     ylim=c(min(df$Eret), max(df$Eret)))
points(df$SD, df$Eret)
ylim = c(0, 1)


print(i)
print(typeof(xs))
print(r145)


# extra test code 

print(double(10))

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

install.packages(microbenchmark)
library(microbenchmark)
microbenchmark(f1(1000), f2(1000), f3(1000), times=5)
system.time(f1(1000))

print(names(a))

print(rr)

print(a)
typeof(a)

typeof(a$P1)
a1 = a$P1
print(a1)

typeof(r1)

r11 <- (a1[-data_len] - a1[-1]) / (a1[-1])
print(r11)
print(r1)

