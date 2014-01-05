chemo <- function(Time, State, Pars) {
         with(as.list(c(State, Pars)), {

         source("callfba.r")

         ub_NH4 <- mu_NH4 * NH4 / (NH4 + ks_NH4) 
         ub_NO3 <- mu_NO3 * NO3 / (NO3 + ks_NO3)
         ub_C   <- mu_C   * C   / (C   + ks_C)
         ub_A   <- Amax
   
         growth.uptake <- callfba(ub_NH4, ub_NO3, ub_C, ub_A)

         g     <- as.numeric(as.character(growth.uptake$g))
         u_NH4 <- as.numeric(as.character(growth.uptake$u_NH4))
         u_NO3 <- as.numeric(as.character(growth.uptake$u_NO3))
         u_C   <- as.numeric(as.character(growth.uptake$u_C))

         dP    <- ( g - D ) * P
         dNH4  <- ( iNH4 - NH4 ) * D - u_NH4 * P 
         dNO3  <- ( iNO3 - NO3 ) * D - u_NO3 * P 
         dC    <- ( iC - C ) * D - u_C * P

         return(list(c(dP, dNH4, dNO3, dC)))
       })
     }



chemo.orig <- function(Time, State, Pars) {
         with(as.list(c(State, Pars)), {

         g     <- N / (1+N)
         u_N   <- 0.1*N / (1+N)
         u_C   <- 0

         dP    <- (g - D ) * P
         dC    <- ( iC - C ) * D - u_C * P
         dN    <- ( iN - N ) * D - u_N * P 


         return(list(c(dP, dN, dC)))
       })
     }

