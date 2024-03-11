rm(list = ls(all.names = TRUE))

library(deSolve)


setwd("~/Automation_Primary_productivity/scripts/lichen_decrease_PP")


# Ajutement des carrying capacite des proies

# delta = 1
# delta = 0.2
# delta = 0.5

# delta = 10

# Ajutement des rep fonctionnelle du loup
# varie entre 0 (0 rep fonc donc 0 loups possible) à 1

# phi = 0.6
phi = 1

Qa = 0
Qj = 0

# on several PP values
source("Auto_gen_PP_delta_related_parameters.R")
# source("Auto_gen_PP_delta_related_parameters_caribou_feuillus.R")
# source("Carrying_capacities_with_deciduous_biomass.R")

# source("Auto_gen_PP_related_parameters.R")

source("Static_vegetation_parameters.R")

# source("Time_dependant_parameters.R")

# source("Parametres_animaux_deers.R")
# 
source("Static_fauna_parameters.R")
# source("Patricia_initial_parameters.R")
# 
# source("Static_fauna_parameters_caribou_feuillus.R")

# source("Evolution_vegetation.R")
source("Evolution_vegetation.R")

# source("test_model_equations.R")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
source("intermediate_res.R")
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# source("intermediate_res_DEBUG.R")

source("make_ODE_function.R")

source("Species_equations.R")

# source("Species_equations_caribou_feuillus.R")
# 


my_function <- function(simulation_index, ...){
  
  result_df <- generate_parameter_dataframe()
  
  
  nested_test <- result_df %>% 
    mutate(pouic = delta) %>% 
    group_by(PP,delta) %>% 
    nest()
  
  
  # list <- list(nested_test$PP, nested_test$data, nested_test$delta)
  
  # Apply the ODE solver
  res <- nested_test %>% 
    # group_by(PP) %>%
    ungroup() %>% 
    mutate(outputs = map2(nested_test$PP, nested_test$data, ~make_ODE(.x, .y))) %>% 
    rename("pouic" = "delta")
  # mutate(outputs = pmap(list, ~make_ODE(..1, ..2, ..3)))
  # mutate(outputs = make_ODE(PP, data))
  
  
  
  saveRDS(res, file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_caribou_moose_deer_additional_prey_wolf.R")
  
  
  
  
  # filename <- paste0("~/Automation_Primary_productivity/res_simulations2/all_simulations_scenario", simulation_index, ".R")
  # saveRDS(res, file = filename)
  
}

simulation_index <- 1

my_function(simulation_index)

# Increment simulation index
# simulation_index <- simulation_index + 1


# my_function(simulation_index)
# 
# # Increment simulation index
# simulation_index <- simulation_index + 1

# test_model_auto <- test$outputs[[1]] %>% 
#   filter(time == 666)
# 
# # Check si les datas sont les memes 
# data_test_auto <- test_model_auto <- test$data[[1]]


