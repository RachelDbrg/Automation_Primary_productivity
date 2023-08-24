
rm(list = ls(all.names = TRUE))

library(deSolve)


setwd("~/Automation_Primary_productivity/scripts")

# on several PP values
source("Auto_gen_PP_related_parameters.R")

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

result_df <- generate_parameter_dataframe()


nested_test <- result_df %>% 
  group_by(PP) %>% 
  nest()


# Apply the ODE solver
res <- nested_test %>% 
  ungroup() %>% 
  mutate(outputs = map2(nested_test$PP, nested_test$data, ~make_ODE(.x, .y)))
  # mutate(outputs = make_ODE(PP, data))

# ter <- res %>% 
#   head(1)

# saveRDS(res, file = "all_simulations.R")
# saveRDS(res, file = "all_simulations_without_deer.R")
saveRDS(res, file = "~/Automation_Primary_productivity/res_simulations/all_simulations_scenario1.R")



# test_model_auto <- test$outputs[[1]] %>% 
#   filter(time == 666)
# 
# # Check si les datas sont les memes 
# data_test_auto <- test_model_auto <- test$data[[1]]
