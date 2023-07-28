library("dplyr")
library("tidyr")
library("purrr")
library("deSolve")

## data omitted for compactness
# test_df_nested <- structure(list(PP = c(0, 0.1, 0.2), data = .....

make_ODE <- function(PP, data){
  
  Unnested_df <-  unnest(data, cols=c())
  
  yini  <- c(V = 1)
  times <- seq(0, 200, by = 1)
  
  # PARS should be one value at a time
  parms  <- c(
    v_croiss = Unnested_df$u_croiss,
    k_V = 100)
  
  actual_ode <- function(yini, times, parms){
    out   <- ode(y = yini,
                 times,
                 equa_diff_sp_test_nest,
                 parms,
                 method = "rk4") %>% 
      as_tibble()
    return(out)
  }
  res <- actual_ode(yini, times, parms)
  return(res)
  # 
}

equa_diff_sp_test_nest <- function(t, y, parms){
  V  <- y[1]
  with(as.list(c(y, parms)), {
    dVdt <- v_croiss * V * (1 - V/k_V)
    return(list(c(dVdt)))
  })
}

test <- test_big_df %>% 
  mutate(outputs = map2(PP, data, ~make_ODE(.x, .y)))
