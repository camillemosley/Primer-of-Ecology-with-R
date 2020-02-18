#chp4
#spatial demographic pulliam-like model

library(primer)
L1 <- 2; L2 <- 0.4
A <- matrix(c(1, 0, L1 - 1, L2), nrow = 2, byrow = TRUE)

#calculating stage structure populations, n1 0.51,n2 0.85, (n1/(n1 + n2)) proportion of total pop in source
eigen(A)

#graphing results, source pop. decreases as lamda increases
L1s <- seq(1, 3, by = 0.01)
p1 <- sapply(L1s, function(l1) {
   A[2, 1] <- l1 - 1
   eigen(A)$vectors[1, 1]/sum(eigen(A)$vectors[, 1])
   })
plot(L1s, p1, type = "l", ylab = "Source Population", xlab = expression(lambda[1]))

#models
#dp/dt=C-E, the change in the proportion of sites occupied is colonization-extinction

#Levins model:dp/dt= cip(1-p)-ep, set of fields invaded by a pest, prop. unoccupied (1-p)
#rate of scattered propagules cip, local extinct rate e, total extinct rate ep
#e and ci(internal conlonization) are dimensionless instentaneous rates, think of as probabilites

levins <- function(t, y, parms) {
   p <- y[1]
   with(as.list(parms), {
     dp <- ci * p * (1 - p) - e * p
     return(list(dp))
     })
   }
library(deSolve)
prms <- c(ci = 0.15, e = 0.05)
Initial.p <- 0.01
out.L <- data.frame(ode(y = Initial.p, times = 1:100, func = levins,parms = prms))
#plotting results of metapopulation model, set levins model to 0 solve for p to find equilibrium
plot(out.L[, 2] ~ out.L[, 1], type = "l", ylim = c(0, 1), ylab = "p", xlab = "time")


#external colonization, open pop
#dp/dt= ce(1-p)-ep

#The propagule rain metapopulation model, island-mainland prop. model, value for deriv. at timepoints
gotelli <- function(t, y, parms) {
   p <- y[1]
   with(as.list(parms), {
     dp <- ce * (1 - p) - e * p
     return(list(dp))
     })
}

#core satellite model takes in to account the rescue effect on extinction(-ep(1-p))
#dp/dt= cip(1-p)-ep(1-p)
hanski <- function(t, y, parms) {
   p <- y[1]
   with(as.list(parms), {
     dp <- ci * p * (1 - p) - e * p * (1 - p)
     return(list(dp))
     })
   }

prms <- c(ci <- 0.15, ce <- 0.15, e = 0.05)
out.IMH <- data.frame(ode(y = Initial.p, times = 1:100,func = gotelli, parms = prms))
out.IMH[["pH"]] <- ode(y = Initial.p, times = 1:100, func = hanski,parms = prms)[, 2]
#plot the results, prop rain and hanski
matplot(out.IMH[, 1], out.IMH[, 2:3], type = "l", col = 1, ylab = "p", xlab = "time")
legend("topleft", c("Hanski", "Propagule Rain"), lty = 2:1,bty = "n")

#calculating equilibrium for the core satellite meta pop model, neg slope = stable equil.
dpdtCS <- expression((ci - e) * p * (1 - p))
ci <- 0.15
e <- 0.05
p <- seq(0, 1, length = 50)
plot(p, eval(dpdtCS), type = "l", ylab = "dp/dt")

#difference between levins and hanksi:Hanski model, K fills all available habitat, 
#whereas the Levins model implies that K fills only a fraction of the total available habitat

#modeling habitat destruction, D is the effect of habitat on overall immigration probability
lande <- function(t, y, parms) {
   p <- y[1]
   with(as.list(parms), {
     dp <- ci * p * (1 - D - p) - e * p
     return(list(dp))
     })
}

#plotting habitat destruction effects
library(deSolve)
prmsD <- c(ci = 0.15, e = 0.05, D = 0)
Ds <- c(0, 0.2, 0.5)
Initial.p <- 0.01
t <- 1:200

#create matrix to hold results
ps <- sapply(Ds, function(d) {
   prmsD["D"] <- d
   ode(y = Initial.p, times = t, func = lande, parms = prmsD)[,+ 2]
   })

#plot results, equilb. set equation to 0 solve for p
matplot(t, ps, type = "l", ylab = "p", xlab = "time")
text(c(200, 200, 200), ps[200, ], paste("D = ", Ds, sep = ""), adj = c(1, 0))

#habitat can shift ci>e to ci<e, levins and hanski both show decline to extinction but hanski has a 
#delyaed extinction,populations influenced by the rescue effect might be prone to unexpected collapse, 
#if the only stable equilibria are 1 and 0

#measuring and plotting unxepceted pop. collapse
C1 <- ode(y = 0.999, times = t, func = hanski, parms = c(ci = 0.2, e = 0.01))
C2 <- ode(y = 0.999, times = t, func = hanski, parms = c(ci = 0.2, e = 0.25))
L2 <- ode(y = 0.95, times = t, func = levins, parms = c(ci = 0.2, e = 0.25))

#plotting different c e relationshiops w/ l and h model
matplot(t, cbind(C1[, 2], C2[, 2], L2[, 2]), type = "l", ylab = "p", xlab = "Time", col = 1)
legend("right", c("c > e", "c < e", "c < e (Levins)"), lty = 1:3, bty = "n")

#questions pdf 142