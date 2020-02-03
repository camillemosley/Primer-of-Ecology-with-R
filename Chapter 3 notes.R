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

                                                     
                                                     
                                                     
                                                                            
