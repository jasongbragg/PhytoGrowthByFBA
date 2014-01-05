
### read model
mod <- readTSVmod(prefix = "microbe", fpath = "/home/jgb/phyflux/r/", fielddelim = "\t", )

callfba <- function(ub_NH4, ub_NO3, ub_C, ub_A) {

  mod.cb <- changeBounds(mod, c(1, 2, 4, 8), ub = c(ub_NH4, ub_NO3, ub_C, ub_A))
  fba.mod.cb <- optimizeProb(mod.cb, algorithm = "fba")
  mtf.mod.cb <- optimizeProb(mod.cb, algorithm = "mtf", wtobj = mod_obj(fba.mod.cb))
  fd.cb <- getFluxDist(mtf.mod.cb)

  growth.uptake <- list(g=fd.cb[10], u_NH4=fd.cb[1], u_NO3=fd.cb[2], u_C=fd.cb[4], red_NO3=fd.cb[5], red_N2=fd.cb[6])  
  return(growth.uptake)
}



