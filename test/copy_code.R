library(tidyverse)

# the data you provided
varst <- as.data.frame(read_csv("var1,var2,var3,var4,var5,var6,var7,var8,var9,var10
1268,1522,1268,1842,4728,5611,5544,2374,1535,5773
1286,1534,1259,1829,4834,5802,5776,2383,1538,5928
1296,1534,1266,1853,4905,5805,5916,2418,1545,5949
1296,1488,1239,1791,4963,5985,5880,2359,1524,6142
1273,1503,1228,1787,4694,5608,5608,2268,1476,5725
1290,1522,1271,1811,4799,5728,5752,2402,1555,5832
1265,1510,1247,1786,4981,6072,6172,2409,1526,6258
1289,1527,1246,1841,4876,5827,5808,2361,1522,6009
1322,1590,1351,1917,4532,5271,5264,2412,1589,5418
1334,1589,1445,1899,3680,4638,4820,2321,1638,4974
1347,1532,1370,1865,3618,4702,4852,2275,1619,4994"))

map_dfc(names(varst), # cycle through each column
        function(x) {         
          # fetch all columns beside x to match
          map(setdiff(names(varst), x),
              function(y){ # your function as above
                v_x <- varst[x]
                v_y <- varst[y]
                ret <- (v_x - v_y) / (v_x + v_y)
                names(ret) <- paste0(x, "/", y)
                ret # return the caluclated values
              })
        })



# -----------------
library("deSolve")

## define as much as possible outside the loop
function1 <- function(times, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    dr = g*r*(1 - (r/k)) - (c*n*r/(1+(h*c*r)))
    dn = (e*c*n*r/(1+(h*c*r)))- n*d
    
    # return the rate of change
    list(c(dr, dn))
  })
}

state <- c(r = 1, n = 1)

parameters = c(g = 1,   # resource growth rate
               d = 0.5, # n mortality rate
               k = 5,   # r carrying capacity
               c = 1,   # consumption rate of n on r
               e = 1,   # conversion efficiency for n on r
               h = 1,    # handling time n on r
               t_kpeak = 1
)
times <- seq(0, 100, by = 1)

## first test single run of model
out <- ode(y = state, times = times, func = function1, parms = parameters)
plot(out)
## It runs and we see a cycling model. I suspect it has no equilibrium!

param.values = seq(from = 1, to = 10, by = 1)

# ==========================================================
t_pertub = 100            
t_low = t_pertub + 5 
# ==========================================================


## define a matrix where results can be stored
sol <- matrix(0, nrow=length(param.values), ncol=2)

for (i in 1:length(param.values)){
  
  ## replace single parameter g with new value
  parameters["t_kpeak"] <- t_low + (50 - 25*param.values[i])
  out <- ode(y = state, times = times, func = function1, parms = parameters)
  
  ## store result of last value in row of matrix.
  ## Note that it may not be an equilibrium
  a <- list(c(parameters["t_kpeak"]))
  return()
}

plot(sol[,1] ~ param.values, type="l")
plot(sol[,2] ~ param.values, type="l")
## We see that the model has no equilibrium.