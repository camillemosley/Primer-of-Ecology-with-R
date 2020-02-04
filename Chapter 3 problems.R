#Chapter 3 problems pg 118 pdf 
#3.1. Dynamics of an annual plant
(a) Calculate rd of an annual plant that has a maximum growth rate of Nt+1/Nt =
  2 at very, very small population sizes.
(b) Calculate the appropriate per capita density dependent effect of an annual
plant with a carrying capacity K of 100 inds·m???2.
(c)Write the appropriate logistic growth equation that incorporates the intrinsic
growth of (a) and the density dependence of (b).
(d) Graph the 10 y dynamics (t = 0, . . . , 10) of the annual plant in (a) and (b),
starting with N0 = 1.

#3.2. Dynamics of E. coli
(a) Calculate r of E. coli that has a doubling time of 30 min. Express this rate
in hours.
(b) Calculate the per capita density dependent effect of an E. coli culture
that grows logistically over a 24 h period, and which levels off at a density
of 107 CFU·mL???1 (CFU is colony forming units - for E. coli its is equivalent
                 to individuals).
(c) Graph the 50 h dynamics (t = 0, . . . , 50) of the E. coli population in (a) and
(b), starting with N0 = 1000.

#3.3. Nonlinear Density Dependence
Sibly et al. [188] found that most species have nonlinear and concave-up density
dependence. They use the -logistic growth model. (a) Create a theta-logistic
crete and continuous logistic growth which assumes linear density dependence.
108 3 Density-dependent Growth
continuous growth model for use with the ode() function in the deSolve package.
(b) Show that with  = 1, it is identical our function clogistic above.
(c) Graph N for t = 0, . . . , 100 using a few different values of  and explain how
 changes logistic growth.

#3.4. Harvested Populations
The logistic growth equation and other similar equations have been used and
abused in efforts to achieve a maximum sustained yield of a harvested population.
The immediate goal of maximum sustained yield management practices
is to kill only the number of individuals that reduces the population to half of
its carrying capacity, assuming that eq. 3.13 describes the population growth.
Answer the questions below to help you explain why this would be a goal.
(a) Find expressions for population growth rates when N = K/4, K/2 3K/4 (substitute
                                                                         these values for N in eq. 3.13, and show your work). Which of these
results in the highest growth rate? How do these relate to the management of
a harvestetd population?
  (b) Show the derivation of the partial derivative of the continuous logistic growth
model, with respect to N (i.e., @ ??N/@N). Solve for zero to determine when total
population growth rate reaches a maximum. Show your work.
(c) What would be the ecological and economic rationale for not killing more
individuals, and keeping N > K/2?
  (d) What would the consequences be for the population if you assume linear
density dependence (1 ??? /N), but in fact the population is governed by nonlinear
density dependence where  < 1 and theta > 1 (Figs. 3.13a-3.13c)?
  (e) What economic benefit would you gain if you harvested the entire population
all at once (and eliminated it from the face of the planet)? What could you
do with all that money?
  (f) How would you incorporate both harvesting and economic considerations
into your logistic growth model?
  
#3.5. Environmental Variability
in predator or pathogen abundance all influence the carrying capacity of the
environment.
(a) Use the discrete version of the logistic growth equation to model a population
in a variable environment. Do this by creating a discrete logistic growth
function that adds (or subtracts) a random amount to K in each time step.
Use one of the many functions that can draw random numbers from particular
distributions (e.g., rpois(), rlnorm(), runif()). You might start by playing
with one of the random number generators:
  Kstart <- 100; time <- 1:20; K <- numeric(20);
for(i in 1:20) K[i] <- Kstart + rnorm(1, m=0, sd=5);
plot(time, K).
(b) All distributions are characterized by their moments. For instance, the Normal
distribution is typically defined by its mean, ??, and variance, 2. Focus on
just one moment of your selected distribution in (a), and use your simulations
Most environments change continually. Temperature, resource availability, changes

#3.6 Summary 109
to determine quantitatively the relation between K and the resulting N derived
from the discrete growth model. For instance, you might vary the standard deviation
of the random normal distribution that contributes to K, and examine
how the standard deviation of K, K relates to mean N, ??N.
(c) Create a reddened time series for K.17(Hint: What are the values of x and
y when you do x <- sample(c(-1,0,1), 20, replace=TRUE); y <- cumsum(x) ?). 
Use this time series to create a reddened population dynamic. (Hint:
                                                                                                                     First create the vector of reddened K's equal in length to your time series.
                                                                                                                   Then create a growth function that can access the vector, e.g. DLG.RK <- function(
                                                                                                                     alpha=0.001, rd=1, N0=10, gens=20, K=K.red). Then, inside the for
                                                                                                                   loop, use an indexed K, such as 1-N[t]/K[t].