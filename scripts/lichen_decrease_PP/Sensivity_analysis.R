# Parameters and ranges can be find at 
# C:\Users\radub33\OneDrive - Université Laval\Model\Hypercube_Latin_bornes.xlsx

library(deSolve)
library(lhs)
library(tidyverse)
library(readxl)


# ==============================================================================
# 1. Load the excel file with the initial values
excel_data <- read_excel("C:/Users/lab/Documents/Automation_Primary_productivity/scripts/PP_only/parameters_LHS.xlsx")
excel_data <- excel_data %>% 
  filter(VALUE != "NA") %>% 
  select(-c(2)) %>% 
  rename("Parameter" = "...1") %>%
  select(Parameter, VALUE)

# parms <- as.list(excel_data)
parameters <- setNames(excel_data$VALUE, excel_data$Parameter)


# Set the variation factor (+/- 15%)
variation_factor <- 0.15

# Create an empty data frame
params_combination <- data.frame(matrix(nrow = 0, ncol = length(parameters)))
colnames(params_combination) <- names(parameters)

# Generate variations for each parameter
for (param_name in names(parameters)) {
  param_value <- parameters[param_name]
  
  # Create a row with original values
  original_row <- parameters
  
  # Increase the parameter value by 15%
  increased_row <- parameters
  increased_row[param_name] <- param_value * (1 + variation_factor)
  
  # Decrease the parameter value by 15%
  decreased_row <- parameters
  decreased_row[param_name] <- param_value * (1 - variation_factor)
  
  # Append the rows to the data frame
  params_combination <- rbind(params_combination, original_row, increased_row, decreased_row)
}

# Add a column with the ID of the scenario
params_combination <- params_combination %>% 
  mutate(sc = 1:nrow(params_combination)) 

# Set the proper names
colnames(params_combination) <- names(parameters)

# # =======================================================================
# 

# Keep if to simplicity as the files "vegetation parameters" and 
# "fauna parameters" work with the "w" vector name

w <- result_df
# 
# # =======================================================================
# phi = 1


# ==============================================================================
# 2. Apply the ODE function, with the proper import of parameters, that comes
# from the previously generated df

for (i in {1:nrow(params_combination)}){

  print(i)
  
# i = 1  
source("Auto_gen_PP_delta_related_parameters.R")
# source("Auto_gen_PP_delta_related_parameters_caribou_feuillus.R")
# source("Carrying_capacities_with_deciduous_biomass.R")

# source("Auto_gen_PP_related_parameters.R")

source("Static_vegetation_parameters.R")

# source("Time_dependant_parameters.R")

# source("Parametres_animaux_deers.R")
# 
  # source("Static_fauna_parameters.R")
source("Fauna_parameters_LHS.R")


# That's where the change happens


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


my_function <- function(i, ... ){
  
  # print(i)
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

  path <- paste0("~/Automation_Primary_productivity/LHS/wo_deer", i, ".RDS")
  print(path)
  
  # saveRDS(res, file = "~/Automation_Primary_productivity/Carrying_capacities/bs.RDS")
  saveRDS(res, file = path)
  # print(simulation_index)
  # simulation_index <- simulation_index + 1
  # print(simulation_index)
  # filename <- paste0("~/Automation_Primary_productivity/res_simulations2/all_simulations_scenario", simulation_index, ".R")
  # saveRDS(res, file = filename)
  # print(c(Moose_init, Caribou_init, Deer_init))
}


# simulation_index <- num_rows 

my_function(i)
}



# ==============================================================================
# 3. Load all the files in the folder, which names starts with a "bs"

# Define the folder path
folder_path <- "~/Automation_Primary_productivity/LHS/wo_deer"

# List files in the folder
file_names <- list.files(folder_path, full.names = TRUE)

# Select files that start with "all_simulations_scenario"
selected_files <- file_names[grep("wo_deer", file_names)]

# Read RDS files and store them in a list
data_list <- lapply(selected_files, readRDS)

# Filter out tibble elements from the list
tibble_elements <- data_list %>% 
  keep(is_tibble)


# Convert each item in data_list to a data frame and assign names
for (i in seq_along(tibble_elements)) {
  df_name <- paste0("wo_deer", i)
  assign(df_name, as.data.frame(tibble_elements[[i]]))
}


# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("wo_deer", all_objects)]

# selected_objects <- list(N1, N2, N3, N4, N5, N6, N7, N8, N9, N10)

# Loop through selected data frames and apply the actions
for (i in seq_along(selected_objects)) {
  obj_name <- selected_objects[i]
  df <- get(obj_name)
  results <- df %>% 
    pull(outputs) %>% 
    map_dfr(as.data.frame) %>% 
    mutate_all(as.numeric) %>% 
    # select(1:10, PP) %>% 
    group_by(PP) %>% 
    # filter(time == 0.00 | time == 800.00) %>%
    # mutate(N = Na + Nj, M = Ma + Mj, C = Ca + Cj) %>% 
    # select(-c(4:7, "Ca", "Cj")) %>%
    mutate(scenario = i) %>% 
    filter(time == 300)
  
  assign(paste0("results_", obj_name), results)
}


# Assume you have data frames named bs_1, bs_2, bs_3, ...
# List all data frames whose names start with "bs"
bs_data_frames <- mget(ls(pattern = "^results_wo_deer"))

# Merge all data frames in the list
merged_df <- Reduce(function(x, y) merge(x, y, all = TRUE), bs_data_frames)

merged_df_wo_deer <- Reduce(function(x, y) merge(x, y, all = TRUE), bs_data_frames)
  
  
# ======
merged_df_wo_deer %>% 
  # filter(PP == "0") %>%
  mutate(
    diff = round((Na + Nj) - (Na[scenario == 1] + Nj[scenario == 1]), 3),
    diff2 = (Na + Nj) - (Na[scenario == 1] + Nj[scenario == 1]),
    fill_color = ifelse(diff == 0, "grey", "red")) %>% 
  ggplot(aes(x = scenario, y = Na, fill = fill_color)) +
  geom_col(color = "black")+
  scale_fill_manual(values = c("red" = "red", "grey" = "grey"), name = "Color")+
  geom_hline(yintercept = Na_value, linetype = "dashed", color = "blue")+
  facet_wrap(~PP)
  
  
Na_value <- merged_df %>% 
  filter(PP == 0 & scenario == 1) %>% 
  select(Na)

Na_value <-  Na_value$Na

# Color in red the final densities that are not equal to the reference sc (= sc1)
# Only look at these scenarios 
merged_df %>% 
  # filter(PP == 0) %>%
  mutate(
    diff = round((Na + Nj) - (Na[scenario == 1] + Nj[scenario == 1]), 3))%>% 
  filter(diff != 0) %>% View()
  group_by(PP) %>% 
  count()
  

  
# Save the file 
saveRDS(merged_df, file = "merged_df_with_deer.rds")
