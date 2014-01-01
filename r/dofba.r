library(sybil)
SYBIL_SETTINGS("SOLVER", "lpSolveAPI", loadPackage = FALSE)

### read model
mod <- readTSVmod(prefix = "microbe", fpath = "/home/jgb/flux/phyto/", fielddelim = "\t", )

### perform fba
fba.mod <- optimizeProb(mod, algorithm = "fba")

### minimize total flux
mtf.mod <- optimizeProb(mod, algorithm = "mtf", wtobj = mod_obj(fba.mod))

#opt <- optimizeProb(microbe, solver = "lpSolveAPI")


pvec <- c(1, 2, 2.5, 5, 7.5, 10, 12.5, 15, 25, 30, 40, 50, 100)
nvec <- c(1, 2, 2.5, 5, 7.5, 10, 12.5, 15, 25, 30, 40, 50, 100)


c <- 1
for (n in pvec)
{

   for (p in nvec)
   {
     
      mod.cb <- changeBounds(mod, c(2, 7), ub = c(n, p))
      fba.mod.cb <- optimizeProb(mod.cb, algorithm = "fba")
      mtf.mod.cb <- optimizeProb(mod.cb, algorithm = "mtf", wtobj = mod_obj(fba.mod.cb))
      fd.cb <- getFluxDist(mtf.mod.cb)
      line <- c(n, p, mod_obj(fba.mod.cb), fd.cb)
      #line <- c(p, n, mod_obj(fba.mod.cb))
      if (c == 1)
      { out <- line }
      if (c > 1)
      { out <- rbind(out, line) }
      c <- c+1
   }

}


### plot as function of NH4 uptake

n1   <- out[,1]==1
n10  <- out[,1]==10
n100 <- out[,1]==100

pdf(file="NFixOnMaxPhotosynth.pdf")
plot(out[ n1 ,2], out[ n1 ,8], ylim=c(0,9), type="l", xlab="max photosynthesis", ylab = "N fixation")
lines(out[ n10 ,2], out[ n10 ,8], type="l", col="green")
lines(out[ n100 ,2], out[ n100 ,8], type="l", col="blue")
dev.off()

pdf(file="GrowthOnMaxPhotosynth.pdf")
plot(out[ n1 ,2], out[ n1 ,3], ylim=c(0,100), type="l", xlab="max photosynthesis", ylab = "growth")
lines(out[ n10 ,2], out[ n10 ,3], type="l", col="green")
lines(out[ n100 ,2], out[ n100 ,3], type="l", col="blue")
dev.off()



p1   <- out[,2]==1
p10  <- out[,2]==10
p100 <- out[,2]==100


pdf(file="NFixOnMaxNH4uptake.pdf")
plot(out[ p1 ,1], out[ p1 ,8], ylim=c(0,9), type="l", xlab="max NH4 uptake", ylab = "N fixation")
lines(out[ p10 ,1], out[ p10 ,8], type="l", col="green")
lines(out[ p100 ,1], out[ p100 ,8], type="l", col="blue")
dev.off()

pdf(file="GrowthOnMaxNH4uptake.pdf")
plot(out[ p1 ,1], out[ p1 ,3], ylim=c(0,100), type="l", xlab="max NH4 uptake", ylab = "growth")
lines(out[ p10 ,1], out[ p10 ,3], type="l", col="green")
lines(out[ p100 ,1], out[ p100 ,3], type="l", col="blue")
dev.off()


