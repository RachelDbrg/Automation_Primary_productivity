# Stack example


test_df <- head(result_df,3)


test_df_nested <- test_df %>% 
  group_by(PP) %>% 
  nest()


make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()
  
  Unnested_df <- test_df_nested %>% 
    unnest(data)
  
  yini  <- c(V = 1)
  # Set the initial value of V = 1
  
  times <- seq(0, 200, by = 1)
  
  
  # PARS should be one value at a time
  parms  <- c(
    # v_croiss = Unnested_df$u_croiss,
    # v_croiss = c(3000,4000,5000), Where the problem occurs
    k_V = kVlow)
  # k_U = kVlow,
  # u_croiss = 2)
  
  actual_ode <- function(yini, times, parms){
    out   <- ode(y = yini,
                 times,
                 equa_diff_sp_test_nest,
                 parms,
                 method = "rk4") %>% 
      as_tibble()
    
    return(out)
    
  }
  
  res <-actual_ode(yini, times, parms)
  return(res)
  
}



equa_diff_sp_test_nest <- function(t,y,parms){
  
  V  <- y[1]

  
  with(as.list(c(y, parms)), {
    

    dVdt <- v_croiss * V * (1 - V/k_V)
    

        return(list(c(dVdt)))
  })
}