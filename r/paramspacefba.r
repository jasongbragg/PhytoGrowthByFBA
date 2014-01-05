library(sybil)
SYBIL_SETTINGS("SOLVER", "lpSolveAPI", loadPackage = FALSE)

source("callfba.r")

### read model
mod <- readTSVmod(prefix = "microbe", fpath = "/home/jgb/phyflux/r/", fielddelim = "\t", )

### some parameters

ks_NH4   = 5           # ks for uptake, micro moles
ks_NO3   = 5           # ks for uptake, micro moles  
ks_C     = 100         # ks for uptake, micro moles
mu_NH4   = 2           # max uptake, micro moles
mu_NO3   = 2           # max uptake, micro moles  
mu_C     = 12          # max uptake, micro moles

NH4vec <- c(0, 0.01, 0.05, 0.1, 0.5, 1, 2.5, 5, 7.5, 10, 25, 50, 100)
Avec   <- c(0.01, 0.05, 0.1, 0.5, 1, 2.5, 5, 7.5, 10, 25, 50, 100, 500, 1200)

c <- 1
for (NH4 in NH4vec)
{

   for (A in Avec)
   {

         NO3 <- 20
         C <- 12
         ub_NH4 <- mu_NH4 * NH4 / (NH4 + ks_NH4) 
         ub_NO3 <- mu_NO3 * NO3 / (NO3 + ks_NO3)
         ub_C   <- mu_C   * C   / (C   + ks_C)
         ub_A   <- A

         growth.uptake <- callfba(ub_NH4, ub_NO3, ub_C, ub_A)

         g     <- as.numeric(as.character(growth.uptake$g))
         u_NH4 <- as.numeric(as.character(growth.uptake$u_NH4))
         u_NO3 <- as.numeric(as.character(growth.uptake$u_NO3))
         u_C   <- as.numeric(as.character(growth.uptake$u_C))
         red_NO3   <- as.numeric(as.character(growth.uptake$red_NO3))
         red_N2    <- as.numeric(as.character(growth.uptake$red_N2))

         line <- c(NH4, NO3, A, g, u_NH4, u_NO3, u_C, red_NO3, red_N2)
         if (c == 1)
         { out <- line }
         if (c > 1)
         { out <- rbind(out, line) }
         c <- c+1
   }

}


### some possible plotting code... 

# n1   <- out[,3]==1
# n10  <- out[,3]==10
# n100 <- out[,3]==100

# pdf(file="NFixOnMaxPhotosynth.pdf")
#  plot(out[ n1, 1], out[ n1 ,4], type="l", xlab="NH4", ylab = "growth")
#  lines(out[ n10 ,1], out[ n10 ,4], type="l", col="green")
#  lines(out[ n100 ,1], out[ n100 ,4], type="l", col="blue")
# dev.off()

### end

