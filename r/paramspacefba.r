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

NH4vec <- c(0.01, 0.05, 0.1, 0.5, 1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 50, 100)
Avec   <- c(0.01, 0.05, 0.1, 0.5, 1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 50)

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

 nh4_sml  <- out[,1]==0.1
 nh4_int  <- out[,1]==2.5

# pdf(file="GrowthOnPhotosynth.pdf")
  plot(out[ nh4_int, 3], out[ nh4_int ,4], type="l", xlab="photoysynthesis", ylab = "growth")
  lines(out[ nh4_sml, 3], out[ nh4_sml ,4], type="l", col="green")
# dev.off()


 nh4_sml  <- out[,1]==1
 nh4_int  <- out[,1]==10

  pdf(file="GrowthOnPhotosynth.pdf")
  plot(out[ nh4_int, 3], out[ nh4_int ,4], type="l", xlab="photoysynthesis", ylab = "growth")
  lines(out[ nh4_sml, 3], out[ nh4_sml ,4], type="l", col="green")
  dev.off()

 pdf(file="NitrateReductionOnPhotosynth.pdf")
 plot(out[ nh4_int, 3], out[ nh4_int ,8], type="l", xlab="photosynthesis", ylab = "NO3 reduction")
 lines(out[ nh4_sml, 3], out[ nh4_sml ,8], type="l", col="green")
 dev.off()
### end

  pdf(file="NitrateReductionOnNH4.pdf")
  a_const <- out[,3]==10
  plot(out[ a_const, 1], out[ a_const ,8], type="l", xlab="NH4 external", ylab = "NO3 reduction")
  dev.off()

