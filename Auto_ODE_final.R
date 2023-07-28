
rm(list = ls(all.names = TRUE))

# on several PP values
source("Auto_gen_PP_related_parameters.R")

source("Static_vegetation_parameters.R")

# source("Time_dependant_parameters.R")

# source("Parametres_animaux_deers.R")

source("Static_fauna_parameters.R")

source("Evolution_vegetation.R")

source("test_model_equations.R")

source("intermediate_res.R")

source("make_ODE_function.R")

source("Species_equations.R")

result_df <- generate_parameter_dataframe()


nested_test <- result_df %>% 
  group_by(PP) %>% 
  nest() %>% 
  head(3)


# Apply the ODE solver
test <- nested_test %>% 
  ungroup() %>% 
  mutate(outputs = map2(nested_test$PP, nested_test$data, ~make_ODE(.x, .y)))
  # mutate(outputs = make_ODE(PP, data))

