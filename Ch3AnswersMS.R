#END OF CHAPTER 3 PROBLEMS
rm(list=ls())
#13.1. Dynamics of an annual plant- annual plant so logistic
#a. Calculate rd of an annual plant that has a maximum growth rate of Nt+1/Nt = 2 at very, very small population sizes.
#N(t+1)=lambda*N(t) --> lambda=2=1+rd so rd=1 UNSURE ABOUT THIS ONE?

#b. Calculate the appropriate per capita density dependent effect of an annual plant with a carrying capacity K of 100inds·m−2.
#K=1000=1/alpha
#per capita effect = rd*(1-Nt/K)
#per capita effect = 1*(1-.001Nt)

#c. Write the appropriate logistic growth equation that incorporates the intrinsic growth of (a) and the density dependence of (b).
#N(t+1)=Nt+1*(1-.001Nt)*Nt

#d. Graph the 10y dynamics (t = 0,..., 10) of the annual plant in (a) and (b), starting with N0 = 1.
dlogisticK<-function(K=1000, rd=1, NO=1, t=10){
  N<-c(NO, numeric(t))
  for (i in 1:t) 
    N[i+1] <-{N[i]+rd*N[i]*(1-N[i]/K)}
  return(N)
}
Nts<-dlogisticK() #uses function w default settings
t=10
plot(0:t, Nts)

#2 Dynamics of E. coli
#a. Calculate r of E. coli that has a doubling time of 30 min. Express this rate in hours.
#tdoubling=ln(2)/r, tdoubling=.5 hr
r=log(2)*.5 #r=.3466

#b. Calculate the per capita density dependent effect of an E. coli culture that grows logistically over a 24 h
#period, and which levels off at a density of 107 CFU·mL−1 (CFU is colony forming units — for E. coli its is equivalent to individuals).
#per capita effect=rd*(1-Nt/K)
#per capita effect=.3466*(1-Nt/(10^7)

#c. Graph the 50h dynamics (t = 0,..., 50) of the E. coli population in (a) and (b), starting with N0 = 1000.
ECol<-dlogisticK(K=10^7, rd=r, NO=1000, t=50)
t<-50
plot(0:t, ECol)

#3. Nonlinear Density Dependence- Sibly et al. [188] found that most species have nonlinear and concave-up density dependence. They use the θ-logistic growth model.
#a. Create a theta-logistic continuous growth model for use with the ode() function in the deSolve package.
thetalogistic<-function(times, y, parms){ #makes function to use w ode
  n<-y[1]
  with(as.list(parms), { #with and as.list creates environment in which R will look inside parms for named elements
    dN.dt<-r*n*(1-(alpha*n)^theta)
    return(list(c(dN.dt)))
  })
}
#THIS IS straight from the text, idk if this is what they're asking for or not

#b. Show that with θ = 1, it is identical our function clogistic above.
prms<-c(r=1, alpha=0.01, theta=1) #creates vectors for parameters
theta=1
t.s<-seq(0.1, 10, by=0.1) #specify time
thetaN<-sapply(theta, function(th){
  prms["theta"]<-th
  ode(y=1, t.s, thetalogistic, prms)[,2]
})
matplot(t.s, thetaN, xlab="Time", ylab="N",type="l") #theta-logistic 

clogistic<-function(times, y, parms){ #function to run R's ODE solver w
  n<-y[1]
  r<-parms[1]
  alpha<-parms[2]         ###KINDA CONFUSED ABOUT THIS STUFF
  dN.dt<-r*n*(1-alpha*n)
  return(list(c(dN.dt)))
}
out<-ode(y=1, times=t.s, clogistic, parms=prms) #output is matrix that includes time steps and the N
lines(out[,1], out[,2], type="b", col="green") #characteristic shape of logistic growth
#same as the theta logistic

#c. Graph N for t = 0,..., 100 using a few different values of θ and explain how θ changes logistic growth.
prms<-c(r=1, alpha=.01, theta=0.5)
theta=0.5
t.s<-(0:100)
thetaN1<-sapply(theta, function(th){ #w theta=0.5
  prms["theta"]<-th
  ode(y=1, t.s, thetalogistic, prms)[,2]
})
matplot(t.s, thetaN1, xlab="Time", ylab="N",type="l", col="green")

prms<-c(r=1, alpha=0.01, theta=1) #theta=1
theta=1
thetaN<-sapply(theta, function(th){
  prms["theta"]<-th
  ode(y=1, t.s, thetalogistic, prms)[,2]
})
lines(t.s, thetaN, col="black")

prms<-c(r=1, alpha=.01, theta=theta) #theta=1.5
theta=1.5
thetaN2<-sapply(theta, function(th){
  prms["theta"]<-th
  ode(y=1, t.s, thetalogistic, prms)[,2]
})
lines(t.s, thetaN2, col="red")
legend("bottomright", c("theta=0.5", "theta=1", "theta=1.5"), text.col=c("green", "black", "red"))

#when theta>1, weakens density dependence at low N so pop grows faster than logistic- red line
#when theta<1, strengthens density dependence at low N so pop grows slower than logistic- green line

#4. 3.4. Harvested Populations- Maximum Sustained Yield
#a. Find expressions for population growth rates when N = K/4, K/2 3K/4 (substitute these values for N in eq. 3.13, and show your work)
#Which of these results in the highest growth rate? How do these relate to the management of a harvested population?
#dN/dt=rN((K-N)/K)
#N=K/4 --> dN/dt=.75rN ***highest growth rate-want to keep N at a value that yields a high growth rate for a harvested population
#N=K/2 --> dN/dt=.5rN
#N=3K/4 --> dN/dt=.25rN

#CONFUSED by this because i thought the max growth rate was at N=K/2, idk

#b. Show the derivation of the partial derivative of the continuous logistic growth model, with respect to N (i.e., ∂N ̇/∂N).
#Solve for zero to determine when total population growth rate reaches a maximum. Show your work.
#NOT sure if this is the right equation?
#dN/dt=rN(1-alpha*N)=r*N-r*alpha*N^2
#dN*/dN=r-2*r*alpha*N = 0
#N=K/2

#c. What would be the ecological and economic rationale for not killing more individuals, and keeping N > K/2?
#ecological- conserving resources/don't want to strain population by keeping it at max growth rate?
#economic- idk

#d. What would the consequences be for the population if you assume linear density dependence (1 − α/N), but in fact the population is 
#governed by nonlinear density dependence where θ < 1 and theta > 1 (Figs. 3.13a-3.13c)?
#when theta>1, density dependence weakens at low N, so population would grow faster than expected
#when theta<1, density dependence strengthens at low N, so population would grow slower than expected- would be bad/pop could go extinct

#e. What economic benefit would you gain if you harvested the entire population all at once (and eliminated it from the face of the planet)? What could you do with all that money?
#if eliminated population, prices would go up for whatever you harvested

#f. How would you incorporate both harvesting and economic considerations into your logistic growth model?
#include a harvesting rate that includes economic stuff/interest rate on investments (like textbook did)

#5.Environmental Variability- rip this one
#a. Use the discrete version of the logistic growth equation to model a population in a variable environment. Do this by creating a discrete logistic growth function that adds (or subtracts) a random amount 
#to K in each time step. Use one of the many functions that can draw random numbers from particular distributions (e.g., rpois(), rlnorm(), runif()). You might start by playing with one of the random number generators:
Kstart <- 100 
time <- 1:20
K <- numeric(20)
for(i in 1:20){
  K[i] <- Kstart + rnorm(1, m=0, sd=5); #rnorm does random generation, mean of 0 and sd of 5
}
plot(time, K)

dlogistic<-function(Kvalue=K, rd=1, NO=2, t=20){
  N<-c(NO, numeric(t))
  for (i in 1:t) 
    N[i+1] <-{N[i]+rd*N[i]*(1-N[i]/Kvalue[i])}
  return(N)
}
Nts<-dlogistic() #uses function w default settings
plot(0:length(time), Nts)

#b. All distributions are characterized by their moments. For instance, the Normal distribution is typically defined by its mean, μ, and variance, σ2. Focus on just one moment of your selected distribution in (a), 
#and use your simulations to determine quantitatively the relation between K and the resulting N derived from the discrete growth model. 
#For instance, you might vary the standard deviation of the random normal distribution that contributes to K, and examine how the standard deviation of K, σK relates to mean N, μN.
#????
avgN<-mean(Nts) #average of previous N growth
Kstart <- 100 
time <- 1:20
K <- numeric(20)
for(i in 1:20){
  K[i] <- Kstart + rnorm(1, m=0, sd=10); #changed standard deviation to 10
}

Nts2<-dlogistic() #uses function w default settings
avgN2<-mean(Nts2)
totaldiff<-Nts-Nts2
diff<-avgN-avgN2 #increasing standard deviation from 5 to 10 on the K distribution only resulted in a difference in 3 on mean N

#c. Create a reddened time series for K.17(Hint: What are the values of x and y when you do x<-sample(c(-1,0,1),20,replace=TRUE);y<-cum- sum(x) ?). 
#Use this time series to create a reddened population dynamic. (Hint: First create the vector of reddened K’s equal in length to your time series. Then create a growth function that can access the vector, e.g. DLG.RK <- function(alpha=0.001, rd=1, N0=10, gens=20, K=K.red).
#Then, inside the for loop, use an indexed K, such as 1-N[t]/K[t].
#WHAT is reddened what
#"reddened environmental variation is variation that is described by a predominance of longer wavelengths and it is a series in which values tend to change gradually,
#where values that are close together in time tend to be more similar, or auto-correlated"
x<-sample(c(-1, 0, 1), 20, replace=TRUE) #samples 20 times from -1, 0, and 1
y<-cumsum(x) #does the cumulative sum of vector x
K.red<-length(y)
Kstart <- 100 
for(i in 1:20){
  K.red[i] <- Kstart + y[i] #IDK if that is what is meant by reddened K's?
}
DLG.RK<-function(alpha=.001, rd=1, NO=10, gens=20, K=K.red){
  N<-c(NO, numeric(t))
  for (i in 1:t) 
    N[i+1] <-{N[i]+rd*N[i]*(1-N[i]/K[i])}
  return(N)
}
Nred<-DLG.RK()
plot(0:t, Nred)
#THIS does not seem correct but idk