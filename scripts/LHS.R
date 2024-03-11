library(deSolve)
library(lhs)
library(tidyverse)

setwd("~/Automation_Primary_productivity/scripts/PP_only")

# First, compute the possible scenarios.
# For each parameter, we would like 100 draw for each value


# install.packages("readxl")
library(readxl)


excel_data <- read_excel("C:/Users/lab/Documents/Automation_Primary_productivity/scripts/PP_only/parameters_LHS.xlsx")
excel_data <- excel_data %>% 
  filter(VALUE != "NA") %>% 
  select(-c(2)) %>% 
  rename("Parameter" = "...1") %>%
  select(Parameter, VALUE)

parms <- as.list(excel_data)
parameters <- setNames(excel_data$VALUE, excel_data$Parameter)

# Variation factor (+/- 15%)
variation_factor <- 0.15

# Create an empty data frame
result_df <- data.frame(matrix(nrow = 0, ncol = length(parameters)))
colnames(result_df) <- names(parameters)

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
  result_df <- rbind(result_df, original_row, increased_row, decreased_row)
}

result_df <- result_df %>% 
  mutate(sc = 1:nrow(result_df)) 


# =======================================================================

w <- result_df 

# =======================================================================
phi = 1

for (i in {1:nrow(w)}){

  print(i)
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

  path <- paste0("~/Automation_Primary_productivity/LHS/bs", i, ".RDS")
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

my_function(i)}



# Test to see if openable
bs1 <- readRDS("~/Automation_Primary_productivity/LHS/bs1.RDS")

bs1 <- bs1 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs1")


# Test to see if openable
bs2 <- readRDS("~/Automation_Primary_productivity/LHS/bs2.RDS")

bs2 <- bs2 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs2")

bs3 <- readRDS("~/Automation_Primary_productivity/LHS/bs3.RDS")

bs3 <- bs3 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs3")

bs4 <- readRDS("~/Automation_Primary_productivity/LHS/bs4.RDS")

bs4 <- bs4 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs4")

bs5 <- readRDS("~/Automation_Primary_productivity/LHS/bs5.RDS")

bs5 <- bs5 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs5")

bs6 <- readRDS("~/Automation_Primary_productivity/LHS/bs6.RDS")

bs6 <- bs6 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs6")

bs7 <- readRDS("~/Automation_Primary_productivity/LHS/bs7.RDS")

bs7 <- bs7 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs7")

bs8 <- readRDS("~/Automation_Primary_productivity/LHS/bs8.RDS")

bs8 <- bs8 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs8")

bs9 <- readRDS("~/Automation_Primary_productivity/LHS/bs9.RDS")

bs9 <- bs9 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs9")

bs1000 <- readRDS("~/Automation_Primary_productivity/LHS/bs1000.RDS")

bs1000 <- bs1000 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "bs1000")


test <- rbind(bs1,bs2)
test <- rbind(test,bs2)
test <- rbind(test,bs3)
test <- rbind(test,bs4)
test <- rbind(test,bs5)
test <- rbind(test,bs6)
test <- rbind(test,bs7)
test <- rbind(test,bs8)
test <- rbind(test,bs9)
test <- rbind(test,bs10)


test %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time, y = density, color = sc))+
  geom_line()+
  facet_grid(species~PP, scales = "free")
