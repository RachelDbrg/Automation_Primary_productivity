library(ODEsensitivity)


# Ref:
# https://cran.r-project.org/web/packages/ODEsensitivity/vignettes/ODEsensitivity.html

# If the assumption of a uniform distribution on the domain intervals 
# doesn’t hold, the Morris screening method cannot be used and the 
# variance-based Sobol’ method should be considered instead. In this case,
# simply switch from using the function ODEmorris to the function ODEsobol.


# MORRIS sensitivity analysis

##### Lotka-Volterra equations #####


LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    
    
    caribou_grazing <- ((a_N * V * (Na+Nj))/(1 + a_N * h_VN * V))
    dVdt <- v_croiss * V * (1 - V/k_V) - caribou_grazing
      

    return(list(c(dVdt)))
  })
}


# The parameters to be included in the sensitivity analysis and their lower
# and upper boundaries:
LVpars <- c("PP", "v_croiss", "k_V")
LVbinf <- c(0.05, 0.05, 0.05 )
LVbsup <- c(1.00, 3.00, 0.95 )

# The initial values of the state variables:
# LVinit <- c(Prey = 1, Predator = 2)
LVinit <- c(V = 1)

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

class(LVres_morris)


plot(LVres_morris)
library(sensitivity)
