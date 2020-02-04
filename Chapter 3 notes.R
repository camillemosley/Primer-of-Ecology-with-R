#Primer in Ecology chapter 3
#Density dependent growth 

library(primer)
#Writing a Function For Discrete Logistic Growth
#An R function will simplify our explorations. It will return a vector of N, given ,
#rd, N0, and t. The function arguments can have defaults (e.g., t=10).
dlogistic <- function(alpha = 0.01, rd = 1, N0 = 2, t = 15) {
   N <- c(N0, numeric(t))
   for (i in 1:t) N[i + 1] <- {
   N[i] + rd * N[i] * (1 - alpha * N[i])
    }
   return(N)
  }

#Graphing Population Size
#We can use the function created above, dlogistic, with default settings, to generate
#a population projection.
Nts <- dlogistic()

#Now we plot the projection, and put in a dotted line for 1/ or K.
t <- 15; a <- 0.01
plot(0:t, Nts)
abline(h = 1/a, lty = 3)

#(Per Capita) Population Growth Increment vs. N (Fig. 3.3)
#Using the previous projection, we now capture both the total and the per capita
#growth increment per unit time, from t to t+1. We graph these versus Nt, population
#size at t.
total.incr <- Nts[1:t + 1] - Nts[1:t]
per.capita.incr <- total.incr/Nts[1:t]
plot(Nts[1:t], total.incr)

plot(Nts[1:t], per.capita.incr)

#Numerical Evaluation of Initial Conditions (Fig. 3.4a)
#Here we draw randomly 30 N0 from a uniform distribution between zero and 1.2K.
#We also include zero specifically. We then use sapply to run dlogistic for each N0,
#using defaults for the other arguments.
N0s <- c(0, runif(30) * 1.1 * 1/a)
N <- sapply(N0s, function(n) dlogistic(N0 = n))
matplot(0:t, N, type = "l", lty = 1, lwd = 0.75, col = 1)
text(t, 1/a, expression(italic("K") == 1/alpha), adj = c(1,0))
                                                         

#Numerical Evaluation of  (Fig. 3.4b)
#Here we draw 30 random K from a uniform distribution from 50 to 1000, and convert
#these to. We use sapply to run dlogistic for each .
 a.s <- 1/runif(30, min = 50, max = 1000)
 N <- sapply(a.s, function(a) dlogistic(alpha = a, t = 15))
#We next plot all populations, and use some fancy code to add some informative text
#in the right locations.
matplot(0:t, N, type = "l", ylim = c(0, 1000), lty = 1, lwd = 0.75,col = 1)
        
text(8, 1/min(a.s), bquote(italic(alpha) == .(round(min(a.s),3))), adj = c(1, 0.5))
                                                     
text(10, 1/max(a.s), bquote(italic(alpha) == .(round(max(a.s),3))), adj = c(0, 1.2))
                                
#Simple Numerical Evaluation of rd (Fig. 3.5)
#Here we vary rd by creating a short systematic sequence rd = 1.3, 1.6, . . . , 2.8. We set
#t = 50, and use dlogistic to create a trajectory for each of the six rd.
rd.v <- seq(1.3, 2.8, by = 0.3)
t <- 15
Ns <- data.frame(sapply(rd.v, function(r) dlogistic(rd = r, t = t)))
matplot(0:t, Ns, type = "l", col = 1)
#Note that many populations do not seem to settle down at K.

#Presentation of Limit Cycles (Fig. 3.6)
#First we make a data frame with the six rd values in the first column, and the
#respective populations in rows, using t() to transpose Ns. This puts the data in
#wide format, with a different time step in each column. (This might, for instance,
#be how you record data in a field experiment with repeated measurements through time).
 tmp <- data.frame(rd = as.factor(rd.v), t(Ns))
#Next, we reshape the data to long format, were all N are in the second column, and
#each is associated with a time step and its rd value (cols. 3 and 4).
 Ns2 <- reshape(tmp, varying = list(2:ncol(tmp)), idvar = "rd",
                v.names = "N", direction = "long")
str(Ns2)
# We plot each trajectory separately using xyplot in a different
#graphics package, lattice. Known as a conditioning plot, xyplot graphs y vs. x
#conditional on g (y x | g).
library(lattice)
print(xyplot(N ~ time | rd, data = Ns2, type = "l", layout = c(3, 2, 1), col = 1))

#Bifurcation Plot: Attractors as a Function of rd (Fig. 3.7)
#Here we perform more comprehensive simulations, and plot the point and periodic
#attractors vs. rd. First we pick some constraints for the simulation: the number of
#different rd, the sequence of rd values, and the number of time steps.
num.rd <- 201; t <- 400
rd.s <- seq(1, 3, length = num.rd)
#Next we use sapply for the simulations (If num.rd and t are large, this could take a while).
 tmp <- sapply(rd.s, function(r) dlogistic(rd = r, N0 = 99,
                                            + t = t))
#Next we convert the output to a data frame and stack up the N in one column. We
#also rename each of the stacked columns, and add new columns for the respective rd
#and time steps.
 tmp.s <- stack(as.data.frame(tmp))
 names(tmp.s) <- c("N", "Old.Column.ID")
 tmp.s$rd <- rep(rd.s, each = t + 1)
 tmp.s$time <- rep(0:t, num.rd)
#We save just the later dynamics in order to focus on the N after they have converged
#on the periodic attractors. Here we select the last 50% of the time steps. (Your figure
#will look a little different than Fig. 3.7 because I used more rd and time steps.)
 N.bif <- subset(tmp.s, time > 0.5 * t)
 plot(N ~ rd, data = N.bif, pch = ".", xlab = quote("r"["d"]))
 
 #Sensitivity to Intitial Conditions
 #We start with three populations, all very close in initial abundance. We then propogate
 #with a rd to generate chaos for 100 time steps.
N.init <- c(97, 98, 99); t <- 30
Ns <- sapply(N.init, function(n0) dlogistic(rd = 2.7, N0 = n0,
                                               + t = t))
#Now we would like to graph them over the first 12 times, and look at the correlations
#between N1 and the other two populations.
  matplot(0:t, Ns, type = "l", col = 1)
  
#Density Dependence on Birth and Death Rates (Figs. 3.9a, 3.9b)
 # To make expressions for eqs. 4.24, 4.17, use expression.
 B.N <- expression(-a * N^2 + e * N - f)
 D.N <- expression(g * N^2)
 #We then provides constants and evaluate the expressions to plot the density dependence
  #on birth and death rates.
  a <- 1/1600; e <- 80 * a; f <- 0.2; g <- 4e-05; N <- 0:100
  plot(N, eval(B.N), type = "l", ylab = "B(N), D(N)")
  lines(N, eval(D.N), lty = 2)
  abline(h = 0, lty = 3)
  legend("bottom", c("Effect on Birth Rate", "Effect on Death Rate"),
         lty = 1:2, bty = "n")
  #The sum of these two rates is merely density dependence,
plot(N, eval(B.N) + eval(D.N), type = "l", ylim = c(-0.4, 1), ylab = "Net Density Dependence, F(N)")
abline(h = 0, lty = 3)
# we can compare this to linear density dependence of the logistic model (1???vN).
 #If v = 0.01, we have
 curve(1 - 0.01 * x, 0, 100, lty = 2, add = T)
 legend("topright", c("Generalized", "Logistic"), lty = 1:2, bty = "n")
 
#Growth rate vs. N
 #We first define an expression, and constants.
 pop.growth.rate <- expression(r * N * (1 - alpha * N))
r <- 1; alpha <- 0.01; N <- 0:120
# A basic plot.
  plot(N, eval(pop.growth.rate), type = "l", ylab = "Population Growth Rate (dN/dt)", xlab = "N")
  abline(h = 0); legend("topright", "r=1", lty = 1)
# Add a few points with labels,
 N <- c(0, 10, 50, 100, 115)
  points(N, eval(pop.growth.rate), cex = 1.5)
  text(N, eval(pop.growth.rate), letters[1:5], adj = c(0.5, 2))
# add arrows for the direction of change in N.
 arrows(20, 2, 80, 2, length = 0.1, lwd = 3)
 arrows(122, -2, 109, -2, length = 0.1, lwd = 3)
 
#Symbolic differentiation
#We can use R's minimal symbolic capabilities to get derivatives. Here we get the
#partial derivative and evaluate for the two equilibria (Fig. 3.10).
  dF.dN <- deriv(pop.growth.rate, "N")
  N <- c(0, 1/alpha)
  eval(dF.dN)
  attr(,"gradient")
 N

#Function for an ODE
# Making a function to use with R's ODE solver is pretty easy, provided we follow
# the rules (see Appendix, secs. , B.10). To make the code transparent, I translate the
# vector parameters and the vector of populations (in this single species model, we
#                                                  have only one population).
  clogistic <- function(times, y, parms) {
     n <- y[1]
     r <- parms[1]
     alpha <- parms[2]
     dN.dt <- r * n * (1 - alpha * n)
     return(list(c(dN.dt)))
     }
# We create vectors for the parameters and the initial densities for all of the populations
 #in the model. We also specify the time.
 prms <- c(r = 1, alpha = 0.01); init.N <- c(1); t.s <- seq(0.1,10, by = 0.1)
#We load the deSolve library, and run ode. The output is a matrix that includes the
 #time steps and the N (Fig. 3.12a).
  library(deSolve)
  out <- ode(y = init.N, times = t.s, clogistic, parms = prms)
  plot(out[, 1], out[, 2], type = "l", xlab = "Time", ylab = "N")
  
#Plotting Random Populations ( (Fig. 3.12b))
 #We use the above function to create 20 populations with different traits. We start
 #with an empty matrix, and then for each of the populations, we draw random N0
 # and r, run the ODE solver, keeping just the column for N. Last we plot the output.
   outmat <- matrix(NA, nrow = length(t.s), ncol = 20)
   for (j in 1:20) outmat[, j] <- {
      y <- runif(n = 1, min = 0, max = 120)
      prms <- c(r = runif(1, 0.01, 2), alpha = 0.01)
      ode(y, times = t.s, clogistic, prms)[, 2]
      }
   matplot(t.s, outmat, type = "l", col = 1, ylab = "All Populations")  
#Theta-logistic function
#Here we make a function that we can use with ode, the numerical integration function.
    thetalogistic <- function(times, y, parms) {
       n <- y[1]
       with(as.list(parms), {
          dN.dt <- r * n * (1 - (alpha * n)^theta)
          return(list(c(dN.dt)))
          })
    }
    
#Theta-logistic density dependence
# We first graph theta-logistic, for theta < 1, theta = 1, and theta > 1 (Fig. 3.13a).
     r <- 0.75; alpha <- 0.01; theta <- c(0.5, 1, 2);
     N <- 0:110
     theta.out <- sapply(theta, function(th) {
        1 - (alpha * N)^th
        })
     matplot(N, theta.out, type = "l", col = 1)
     abline(h = 0)
     legend("topright", legend = paste("theta =", c(2, 1, 0.5)),lty = 3:1)
     
#Theta-logistic growth rate
#We plot the growth rate (a.k.a. the production function) for the theta-logistic model
#with theta < 1, theta = 1, and theta > 1 (Fig. 3.13b).
      thetaGR.out <- sapply(theta, function(th) {
         r * N * (1 - (alpha * N)^th)
         })
      matplot(N, thetaGR.out, type = "l", col = 1)
      abline(h = 0)
      legend("bottomleft", legend = paste("theta =", c(2, 1, 0.5)),lty = 3:1)
#We also add an example of growth with low theta, but higher r.
     
#Theta-logistic dynamics
#We solve numerically for N, and plot the dynamics for N with theta < 1, theta = 1, and theta > 1
      prms <- c(r <- 0.75, alpha <- 0.01, theta = 1)
      thetaN <- sapply(theta, function(th) {
         prms["theta"] <- th
         ode(y = 1, t.s, thetalogistic, prms)[, 2]
         })
      matplot(t.s, thetaN, type = "l")
      legend("topleft", legend = paste("theta =", c(2, 1, 0.5)), lty = 3:1, bty = "n")
      
 #MSY and harvesting (Fig. 3.14a)
#Here we illustrate the interaction harvesting at a rate associated with MSY for
#logistic growth. We set logistic model parameters, and first plot logistic growth
# without harvest.
    r <- 0.5; alpha <- 0.01; N <- 0:105
    plot(N, eval(pop.growth.rate), type = "l", ylim = c(-3, 15), ylab = "dN/dt and FN")
    abline(h = 0)
# We then calculate F based on our solution eq. 9.6, and plot the linear harvest function
# with an intercept of zero, and slope of F.
    F <- r/2
    abline(c(0, F), lty = 2)
#Equilibrium solution for logistic growth with harvesting (Fig. 3.14b)
#When we add harvesting at rate F = r/2, we get a new equilibrium. Here we illustrate
#this using the same parameters as above, but now using the entire function with both
#growth and harvest.
       pgr.H <- expression(r * N * (1 - alpha * N) - F * N)
       N <- 0:55
       plot(N, eval(pgr.H), type = "l", ylab = "dN/dt (growth - harvesting)")
       abline(h = 0)
#This merely represents the new equilibrium (where dN/dt crosses the x-axis) with a
#harvest of rate F = r/2.     
     
