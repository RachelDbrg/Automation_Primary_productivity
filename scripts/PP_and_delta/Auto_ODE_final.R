
rm(list = ls(all.names = TRUE))

library(deSolve)


setwd("~/Automation_Primary_productivity/scripts/PP_and_delta")


# Ajutement des carrying capacite des proies

# delta = 1
# delta = 0.2
# delta = 0.5

# delta = 10

# Ajutement des rep fonctionnelle du loup
# varie entre 0 (0 rep fonc donc 0 loups possible) à 1

phi = 0.6

# on several PP values
source("Auto_gen_PP_delta_related_parameters.R")
# source("Auto_gen_PP_related_parameters.R")

source("Static_vegetation_parameters.R")

# source("Time_dependant_parameters.R")

# source("Parametres_animaux_deers.R")

source("Static_fauna_parameters.R")

source("Evolution_vegetation.R")

# source("test_model_equations.R")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
source("intermediate_res.R")
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# source("intermediate_res_DEBUG.R")

source("make_ODE_function.R")

source("Species_equations.R")



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

# ter <- res %>% 
#   head(1)
# ?pmap

# EST
# saveRDS(res, file = "all_simulations_long_period.R")
# saveRDS(res, file = "all_simulations_without_deer_long_period.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/high_density.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/extra_low_density.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/average_density.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/low8_density.R")

# OUEST
# saveRDS(res, file = "all_simulations_long_period.R")
# saveRDS(res, file = "all_simulations_without_deer_long_period.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/high_density_west.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/extra_low_density_west.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/average_density_west.R")

# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/extra_low_density_west.R")

# saveRDS(res, file = "~/Automation_Primary_productivity/Varying_prey_density/low8_density_west.R")

# saveRDS(res, file = "~/Automation_Primary_productivity/Messier_figure/sim7.R")
saveRDS(res, file = "~/Automation_Primary_productivity/Messier_figure/After_correction/delta_PP.R")


# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario1.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario2.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario4.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario5.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario6.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario7.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario8.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario9.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario10.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario11.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario12.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario13.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario14.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario15.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario16.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario17.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario19.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario20.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario21.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario22.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario23.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario24.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario25.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario26.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario27.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario28.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario29.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario30.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario31.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario32.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario33.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario34.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario35.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario36.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario37.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario38.R")
# saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario39.R")


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
