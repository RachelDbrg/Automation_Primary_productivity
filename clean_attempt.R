# Call the function computing vegetation parameters based 
# on several PP values
source("Auto_gen_PP_related_parameters.R")

# Call the parameters that are assumed to not depend on PP
source("Static_vegetation_parameters.R")

# source("Parametres_animaux_deers.R")

source("Static_fauna_parameters.R")

# source("Evolution_vegetation.R")

source("test_model_equations.R")

source("intermediate_res.R")


result_df <- generate_parameter_dataframe()

time_dep <- rte()



# WORKS FOR ON ROW  ----------------------------------------
test <- result_df[1:3,]


make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()

  yini  <- c(V = 1)
  # Set the initial value of V = 1
  
  times <- seq(0, 200, by = 1)
  

  # PARS should be one value at a time
  parms  <- c(
    t_low = 1
  )
  # v_croiss = Unnested_df$u_croiss,
  # v_croiss = c(3000,4000,5000), Where the problem occurs
  # k_V = kVlow)
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
  

test_df_nested <- test %>% 
  group_by(PP) %>% 
  nest()

test <- test_df_nested %>% 
  mutate(outputs = map2(PP, data, ~make_ODE(.x, .y)))



# -----------------------------------------------------------
test <- result_df[1,]

t_perturb <- 100


make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()
  
  yini  <- c(V = 1)
  # Set the initial value of V = 1
  
  times <- seq(0, 200, by = 1)
  
  
  # PARS should be one value at a time
  parms  <- c()
  # v_croiss = Unnested_df$u_croiss,
  # v_croiss = c(3000,4000,5000), Where the problem occurs
  # k_V = kVlow)
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


test_df_nested <- test %>% 
  group_by(PP) %>% 
  nest()

test <- test_df_nested %>% 
  mutate(outputs = map2(PP, data, ~make_ODE(.x, .y)))
