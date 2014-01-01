chemo <- function(Time, State, Pars) {
         with(as.list(c(State, Pars)), {

         source("callfba.r")

         growth.uptake <- callfba(C, N)

         g <- as.numeric(as.character(growth.uptake$g))
         u_N <- as.numeric(as.character(growth.uptake$u_N))
         u_C <- as.numeric(as.character(growth.uptake$u_C))

         dP    <- (g - D ) * P
         dC    <- ( iC - C ) * D - u_C * P
         dN    <- ( iN - N ) * D - u_N * P 


         return(list(c(dP, dN, dC)))
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

