library(deSolve)
CSFBA <- function(Time, State, Pars) {
         with(as.list(c(State, Pars)), {

         g     <- N / (1+N)
         u_N   <- 0.1*N / (1+N)
         u_C   <- 0

         dP    <- (g - D ) * P
         dC    <- ( iC - C ) * D #- u_C * P
         dN    <- ( iN - N ) * D - u_N * P 


         return(list(c(dP, dN, dC)))
       })
     }
     
     pars  <- c(D      = 1.0,    # dilution rate
                iN     = 2.0,  # inflow N concentration
                iC     = 5.0)  # inflow C concentration 
               
     
     yini  <- c(P = 10, N = 20, C = 1)
     times <- seq(0, 20, by = 0.1)
     out   <- ode(yini, times, CSFBA, pars)
     summary(out)
     
     ## Default plot method
     plot(out)

