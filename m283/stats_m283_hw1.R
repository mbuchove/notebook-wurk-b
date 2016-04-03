v1 = 0.0061
v2 = 0.0046
s1 = sqrt(v1)
s2 = sqrt(v2)

rho <- function(x1) {(v2-x1^2*v1-(1-x1)^2*v2)/(x1*(1-x1)*sqrt(v1)*sqrt(v2))}
curve(rho, from=0, to=1, xname="x1", yname="rho_min")
optimize(rho, interval=c(0,1), maximum=TRUE)
print(sqrt(v2/v1))
