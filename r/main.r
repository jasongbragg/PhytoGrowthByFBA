## call libraries
library(deSolve)

## call functions
source("equations.r")

library(sybil)
SYBIL_SETTINGS("SOLVER", "lpSolveAPI", loadPackage = TRUE)

### read model
mod <- readTSVmod(prefix = "microbe", fpath = "/home/jgb/phyflux/r/", fielddelim = "\t", )

## initial conditions
yini  <- c(P = 10,      # protein, in micro moles of N per litre
           NH4 = 0.1,   # NH4, in micro moles per litre
           NO3 = 0,     # NO3, in micro moles per litre
           C = 10)      # inorganic carbon, in micromoles per litre

## time steps
tzero <- 0              # start time, in seconds
tmax  <- 4 * 86400     # stop time, in seconds
step  <- 1 * 86400   # time step, in seconds 

times <- seq(tzero, tmax, by = step)

## parameters
pars  <- c(D        = 8.023E-6 , # dilution rate 1/s
           iNH4     = 10.0,        # inflow NH4 concentration micro moles
           iNO3     = 0.0,         # inflow NH4 concentration micro moles
           iC       = 15.0,        # inflow C concentration micro moles
           ks_NH4   = 5,           # ks for uptake, micro moles
           ks_NO3   = 5,           # ks for uptake, micro moles  
           ks_C     = 100,         # ks for uptake, micro moles
           mu_NH4   = 2,           # max uptake, micro moles
           mu_NO3   = 2,           # max uptake, micro moles  
           mu_C     = 12,          # max uptake, micro moles
           Amax     = 100)         # max rate of photosynthesis    

## call the solver
solutions   <- ode(yini, times, chemo, pars)
               
## Default plot method
#summary(solutions)
plot(solutions)

