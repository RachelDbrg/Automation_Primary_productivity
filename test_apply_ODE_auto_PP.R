
# ------------------ WORKS ---------------------------

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

# Call the function to get the data frame
# initialise the values of PP-dependent parameter
# for each PP values
result_df <- generate_parameter_dataframe()

test_df <- result_df[1,]

# In "result_df" are all the scenarios that I want to 
# run the ode function on. 

pars  <- list(
  v_croiss = v_croiss,
  k_V = kVlow,
  u_croiss = test_df$u_croiss,
  k_U = test_df$kUpeak #NOT GOOD
)


yini  <- c(V = 1, U = 2)
times <- seq(0, 200, by = 1)
out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  as.data.frame()

# Add the test_df so we can keep track of the values used

res_fin <- tibble(
  PP = test_df$PP,
  
  init = test_df_nested$data,
  
  ouputs = list(out)
)
# This is good. Should stay. 

# Map function should do it.


# -------- USING MAP FUNCTION - DOES NOT WORK --------------

test_df <- result_df[1:3,]
test_df <- result_df[1,]


# Our base dataframe
test_df_nested <- test_df %>% 
  group_by(PP) %>% 
  nest()


make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()
  
  Unnested_df <- test_df_nested %>% 
    unnest(data)
  
  yini  <- y0_test
  # For now = 2 values
  
  times <- seq(0, 200, by = 1)
  
  
  # PARS should be one value at a time
  pars  <- list(
    v_croiss = v_croiss,
    k_V = kVlow,
    u_croiss = Unnested_df$u_croiss,
    k_U = Unnested_df$kUpeak)
  
  actual_ode <- function(yini, times, pars){
    out   <- ode(y = yini,
                 times,
                 equa_diff_sp_test,
                 pars,
                 method = "rk4") %>% 
      as_tibble()
  
    return(out)
    
  }
  
  res <-actual_ode(yini, times, pars)
  return(res)
  
}

make_ODE(test_df_nested$data)


# test <- test_df_nested %>% 
#   mutate(outputs = pmap(list(PP,data), ~make_ODE()))


test <- test_df_nested %>% 
  ungroup() %>% 
  mutate(outputs = map(data, ~make_ODE(.x)))






test_df <- result_df[1:3,]


test_df_nested %>% 
  mutate(PP = as.factor(PP))


test <- test_df_nested %>% 
  mutate(outputs = group_map(data, ~make_ODE(.x)))

test %>% 
  group_by(PP) %>% 
  group_map(~make_ODE())



# SHOULD LOOP FOR EVERY PP VALUES BUT DOES NOT


# TRASH FONCTION - WORKS

test_df <- result_df[1:2,]

test_df_nested <- test_df %>% 
  group_by(PP) %>% 
  nest()


test_fct <- function(data){
  som = sum(data)
  prod = prod(data)
  
  res <- tibble(som, prod)
  return(res)
}


test <- test_df_nested %>% 
  mutate(outputs = map(data, ~test_fct(.)))

# --------------------------------------



test_df <- result_df[1:3,]
test_df <- result_df[1,]


# Our base dataframe
test_df_nested <- test_df %>% 
  group_by(PP) %>% 
  nest()


make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()
  
  # yini  <- y0_test
  yini  <- 2
  # For now = 2 values
  
  times <- seq(0, 200, by = 1)
  
  pars  <- list(
    v_croiss = v_croiss,
    k_V = kVlow,
    u_croiss = test_df$u_croiss,
    k_U = test_df$kUpeak #NOT GOOD
  )
  
  out   <- ode(y = yini,
               times,
               trash,
               pars,
               method = "rk4")
  
  return(out)
}

make_ODE()


# test <- test_df_nested %>% 
#   mutate(outputs = map2(PP,data, ~make_ODE(.x, .y)))


test <- test_df_nested %>% 
  mutate(outputs = map(data, ~test_fct(.x)))





# ------------------------------------------------

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



# Works for fixed values of parameters

make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()
  
  Unnested_df <- test_df_nested %>% 
    unnest(data)
  
  yini  <- c(V = 2)
  # For now = 2 values
  
  times <- seq(0, 200, by = 1)
  
  
  # PARS should be one value at a time
  parms  <- c(
    v_croiss = v_croiss,
    k_V = kVlow)
    # k_U = kVlow,
    # u_croiss = 2)
  
  actual_ode <- function(yini, times, parms){
    out   <- ode(y = yini,
                 times,
                 equa_diff_sp_test_nest,
                 pars,
                 method = "rk4") %>% 
      as_tibble()
    
    return(out)
    
  }
  
  res <-actual_ode(yini, times, parms)
  return(res)
  
}


test_df <- result_df[1:3,]


test_df_nested <- test_df %>% 
  group_by(PP) %>% 
  nest()

test <- test_df_nested %>% 
  mutate(outputs = map(data, ~make_ODE(.x)))




# ----------------------------------------


# Does the problem occurs for changing values of parameters?

make_ODE <- function(PP,data){
  
  # out   <- ode(yini, times, equa_diff_sp_test, pars, method = "rk4") %>% 
  #   as.data.frame()
  
  Unnested_df <- test_df_nested %>% 
    unnest(data)
  
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


test_df <- result_df[1:3,]


test_df_nested <- test_df %>% 
  group_by(PP) %>% 
  nest()

test <- test_df_nested %>% 
  mutate(outputs = map2(PP, data, ~make_ODE(.x, .y)))


