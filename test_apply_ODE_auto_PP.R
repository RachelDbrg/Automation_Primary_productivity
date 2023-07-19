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
out   <- ode(yini, times, equa_diff_sp_test, pars) %>% 
  as.data.frame()
