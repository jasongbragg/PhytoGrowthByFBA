library(sybil)
SYBIL_SETTINGS("SOLVER", "lpSolveAPI", loadPackage = TRUE)

### read model
mod <- readTSVmod(prefix = "microbe", fpath = "/home/jgb/phyflux/r/", fielddelim = "\t", )

callfba <- function(C, N) {

  mod.cb <- changeBounds(mod, c(2, 3), ub = c(N, C))
  fba.mod.cb <- optimizeProb(mod.cb, algorithm = "fba")
  mtf.mod.cb <- optimizeProb(mod.cb, algorithm = "mtf", wtobj = mod_obj(fba.mod.cb))
  fd.cb <- getFluxDist(mtf.mod.cb)

  growth.uptake <- list(g=fd.cb[8], u_N=fd.cb[2], u_C=fd.cb[3] )  
  return(growth.uptake)
}



