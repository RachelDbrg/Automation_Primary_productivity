library(ODEsensitivity)


# Ref:
# https://cran.r-project.org/web/packages/ODEsensitivity/vignettes/ODEsensitivity.html

# If the assumption of a uniform distribution on the domain intervals 
# doesn’t hold, the Morris screening method cannot be used and the 
# variance-based Sobol’ method should be considered instead. In this case,
# simply switch from using the function ODEmorris to the function ODEsobol.


##### Lotka-Volterra equations #####
# The model function:
LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    Ingestion <- rIng * Prey * Predator
    GrowthPrey <- rGrow * Prey * (1 - Prey/K)
    MortPredator <- rMort * Predator
    dPrey <- GrowthPrey - Ingestion
    dPredator <- Ingestion * assEff - MortPredator
    return(list(c(dPrey, dPredator)))
  })
}


# The parameters to be included in the sensitivity analysis and their lower
# and upper boundaries:
LVpars <- c("rIng", "rGrow", "rMort", "assEff", "K")
LVbinf <- c(0.05, 0.05, 0.05, 0.05, 1)
LVbsup <- c(1.00, 3.00, 0.95, 0.95, 20)

# The initial values of the state variables:
LVinit <- c(Prey = 1, Predator = 2)

# The timepoints of interest:
LVtimes <- c(0.01, seq(1, 10, by = 1))

# Morris screening:
set.seed(7292)
# Warning: The following code might take very long!
LVres_morris <- ODEmorris(mod = LVmod,
                          pars = LVpars,
                          state_init = LVinit,
                          times = LVtimes,
                          binf = LVbinf,
                          bsup = LVbsup,
                          r = 500,
                          design = list(type = "oat",
                                        levels = 10, grid.jump = 1),
                          scale = TRUE,
                          ode_method = "lsoda",
                          parallel_eval = TRUE,
                          parallel_eval_ncores = 2)





# SOBOL
##### Lotka-Volterra equations #####
# The model function:
LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    Ingestion <- rIng * Prey * Predator
    GrowthPrey <- rGrow * Prey * (1 - Prey/K)
    MortPredator <- rMort * Predator
    dPrey <- GrowthPrey - Ingestion
    dPredator <- Ingestion * assEff - MortPredator
    return(list(c(dPrey, dPredator)))
  })
}
# The parameters to be included in the sensitivity analysis and their lower
# and upper boundaries:
LVpars <- c("rIng", "rGrow", "rMort", "assEff", "K")
LVbinf <- c(0.05, 0.05, 0.05, 0.05, 1)
LVbsup <- c(1.00, 3.00, 0.95, 0.95, 20)
# The initial values of the state variables:
LVinit <- c(Prey = 1, Predator = 2)
# The timepoints of interest:
LVtimes <- c(0.01, seq(1, 50, by = 1))
set.seed(59281)
# Sobol' sensitivity analysis (here only with n = 500, but n = 1000 is
# recommended):
# Warning: The following code might take very long!
LVres_sobol <- ODEsobol(mod = LVmod,pars = LVpars,
                        state_init = LVinit,
                        times = LVtimes,
                        n = 500,
                        rfuncs = "runif",
                        rargs = paste0("min = ", LVbinf,
                                       ", max = ", LVbsup),
                        sobol_method = "Martinez",
                        ode_method = "lsoda",
                        parallel_eval = TRUE,
                        parallel_eval_ncores = 2)



plot(LVres_sobol)
