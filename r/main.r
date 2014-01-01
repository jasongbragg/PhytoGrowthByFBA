## call libraries
library(deSolve)

## call functions
source("equations.r")

## initial conditions
yini  <- c(P = 2, N = 4, C = 4)

## time steps
tzero <- 0
tmax  <- 10
step  <- 0.25

times <- seq(tzero, tmax, by = step)

## parameters
pars  <- c(D      = 0.5,    # dilution rate
           iN     = 2.0,    # inflow N concentration
           iC     = 1.5)    # inflow C concentration 


## call the solver
solutions   <- ode(yini, times, chemo, pars)

               
## Default plot method
summary(solutions)
plot(solutions)

