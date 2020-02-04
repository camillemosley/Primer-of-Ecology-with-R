#Chapter 3 problem
#3.1 
#a) rd of an annual plant that has a maximum growth rate of Nt+1/Nt=2 at very, very small population sizes
rd = 1
#b)calculate alpha
alpha = 1/100
#c)calculate logisitc growth equation
dlogistic <- function(alpha,rd,N0,t) { 
  N = c(N0, numeric(t))
  for(i in 1:t) {
    N[i+1] = N[i] + rd * N[i] * (1 - alpha * N[i])
  }
  return(N)
}
#d) graph for 10 years
dlogis = dlogistic(alpha, rd, 1, 10)
plot(xlab="years",ylab = "growth", x = 0:10,dlogis)

#3.2
#a)find doubling time for 30 minutes for e.coli
r= 60*(log(2)/30)
#b)calculate per captia density 
alpha = 1/107
#c)graph 50 hour dynamics 

#3.3
#a)
thetalogistic <- function(times, y, parms) {
  n = y[1]
  with(as.list(parms), {
    dN.dt = r * n * (1 - (alpha * n)^theta)
    return(list(c(dN.dt)))
  })
}

#b)show that theta=1
prms = c(r = r, alpha = alpha)
init.N = c(1000)
t.s = seq(0.1,50, by = 1)
out = ode(y = init.N, times = t.s, clogistic, parms = prms)
plot(out[, 1], out[, 2], type = "l", xlab = "Time", ylab = "N")
parms = c(r=r,)

r <- 0.75 
alpha <- 0.01 
theta <- c(0.5, 1, 2)
N <- 0:110
theta.out <- sapply(theta, function(th) {
       1 - (alpha * N)^th
   })
 matplot(N, theta.out, ylab="theta",type = "l", col = 1)
 abline(h = 0)
 legend("bottomleft", legend = paste("theta =", c(2, 1, 0.5)), lty = 3:1)









