# sc38 <- readRDS("~/Automation_Primary_productivity/res_simulations/all_simulations_scenario38.R")

library(tidyverse)

# Define the folder path
folder_path <- "~/Automation_Primary_productivity/res_simulations2"

# List files in the folder
file_names <- list.files(folder_path, full.names = TRUE)

# Select files that start with "all_simulations_scenario"
selected_files <- file_names[grep("all_simulations_scenario", file_names)]

# Read RDS files and store them in a list
data_list <- lapply(selected_files, readRDS)

# Filter out tibble elements from the list
tibble_elements <- data_list %>% 
  keep(is_tibble)


# Convert each item in data_list to a data frame and assign names
for (i in seq_along(tibble_elements)) {
  df_name <- paste0("sc", i)
  assign(df_name, as.data.frame(tibble_elements[[i]]))
}


# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("^sc", all_objects)]

# Loop through selected data frames and apply the actions
for (i in seq_along(selected_objects)) {
  obj_name <- selected_objects[i]
  df <- get(obj_name)
  results <- df %>% 
    pull(outputs) %>% 
    map_dfr(as.data.frame) %>% 
    mutate_all(as.numeric) %>% 
    group_by(PP) %>% 
    filter(time == 0.00 | time == 800.00) %>%
    select(1:10, PP) %>% 
    mutate(N = Na + Nj, M = Ma + Mj, C = Ca + Cj) %>% 
    select(-c(4:7, "Ca", "Cj")) %>%
    mutate(scenario = i)
  
  assign(paste0("results_", obj_name), results)
}

qweqwrt <- sc1 %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  group_by(PP) %>%
  filter(time == 0.00 | time == 800.00) %>%
  select(1:10, PP) %>%
  mutate(N_init = Na + Nj, M_init = Ma + Mj, C_init = Ca + Cj) %>%
  select(-c(4:6, "Ca", "Cj")) %>%
  mutate(scenario = i)


dasfg <- sc1 %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) 


# Colonnes spécifiques que vous souhaitez conserver
specific_columns <- c("time", "U", "V", "Na", "Nj", 
                      "Ca", "Cj", "P", "Ma", "Mj")


pattern <- "rfonc"

# Use grep to find column names matching the pattern
matching_columns <- grep(pattern, names(dasfg), value = TRUE)


# Utilisez la fonction subset pour sélectionner les colonnes spécifiques et celles correspondant au motif regex
selected_data <- subset(dasfg, select = c(specific_columns, grep(pattern, names(dasfg), value = TRUE)))


qwr <- selected_data %>% 
  head(801) %>% 
  mutate(M = Ma+Mj) %>% 
  ggplot(aes(x = M,
             y = rfonc_P_Ma))+
  geom_point()

qwr


# Merge all scenarios in one df
# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("^results_sc", all_objects)]

# Merge the selected data frames
merged_df <- do.call(rbind, lapply(selected_objects, get))


description_scenarios <- merged_df %>% 
  filter(time == 0.00,
         PP == 0)

short_description_scenarios <- merged_df %>% 
  filter(time == 0.00) %>% 
  group_by(time, U, V, P, PP, N, M) %>% 
  nest() %>% 
  mutate(row_count = purrr::map_int(data, nrow))


# Comparaison avec et sans cerfs

compar <- merged_df %>% 
  filter(time == 0.00) %>% 
  group_by(V,U,P,PP,M,N) %>% 
  select(-c(scenario)) %>% 
  nest()



# Check if every row of the nested df as different deer densities
df_with_count <- test %>%
  mutate(row_count = purrr::map_int(data, nrow))


long_comparison <- merged_df %>% 
  mutate(status = ifelse(time == 0, "initial", ifelse(time == 800, "final", NA))) %>% 
  group_by(scenario) %>% 
  nest() %>% 
  mutate(data = map(data, ~ .x %>%
                      pivot_longer(cols = c(V,U,P,N,M,C),
                                   names_to = "species",
                                   values_to = "density")))


test_plot <- long_comparison %>% 
  head(1) %>% 
  pull(data) %>% 
  map_dfr(as.data.frame)


test_plot %>% 
  ggplot(aes(x = time,
             y = density,
             color = as.factor(PP)))+
  geom_line()+
  facet_wrap(~species, scales = "free")



# ==============================================================================
# For same initial values, what does initial values of deer changes in the 
# final densities?

growth_rate <- merged_df %>% 
  group_by(PP, scenario) %>% 
  mutate(Diff_time = time - lag(time),  # Difference in time (just in case there are gaps)
         Diff_growth_N = N - lag(N), # Difference in route between years
         Rate_percent_N = (Diff_growth_N / Diff_time)/N * 100,
         initial_N = lag(N)) # growth rate in percent


growth_rate %>% 
  ggplot(aes(x = PP,
             y = Rate_percent_N,
             color = initial_N))+
  geom_point()



qwer_wo_deer <- sc1 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(N = Na + Nj,
         M = Ma + Mj,
         C = Ca + Cj) %>% 
  pivot_longer(cols = c(V,U,P,N,M,C),
               names_to = "species",
               values_to = "density")


qwer_wo_deer %>% 
  select(time, species, density, PP) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(species)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")



qwer_w_deer <- sc2 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(N = Na + Nj,
         M = Ma + Mj,
         C = Ca + Cj) %>% 
  pivot_longer(cols = c(V,U,P,N,M,C),
               names_to = "species",
               values_to = "density")


qwer_w_deer %>% 
  select(time, species, density, PP) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(species)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")




# ==============================================================================

library(tidyverse)


# TIME SERIES

# Define the folder path
folder_path <- "~/Automation_Primary_productivity/res_simulations2"

# List files in the folder
file_names <- list.files(folder_path, full.names = TRUE)

# Select files that start with "all_simulations_scenario"
selected_files <- file_names[grep("all_simulations_scenario", file_names)]

# Read RDS files and store them in a list
data_list <- lapply(selected_files, readRDS)

# Filter out tibble elements from the list
tibble_elements <- data_list %>% 
  keep(is_tibble)


# Convert each item in data_list to a data frame and assign names
for (i in seq_along(tibble_elements)) {
  df_name <- paste0("sc", i)
  assign(df_name, as.data.frame(tibble_elements[[i]]))
}


# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("^sc", all_objects)]

# Loop through selected data frames and apply the actions
for (i in seq_along(selected_objects)) {
  obj_name <- selected_objects[i]
  df <- get(obj_name)
  results <- df %>% 
    pull(outputs) %>% 
    map_dfr(as.data.frame) %>% 
    mutate_all(as.numeric) %>% 
    group_by(PP) %>% 
    # filter(time == 0.00 | time == 800.00) %>%
    select(1:10, PP) %>% 
    mutate(N = Na + Nj, M = Ma + Mj, C = Ca + Cj) %>% 
    select(-c(4:7, "Ca", "Cj")) %>%
    mutate(scenario = i)
  
  assign(paste0("results_", obj_name), results)
}


# Merge all scenarios in one df
# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("^results_sc", all_objects)]

# Merge the selected data frames
merged_df_full <- do.call(rbind, lapply(selected_objects, get))




# Gather in sub df all simulations that have the same initial conditions

similar_scenarios <- merged_df_full %>% 
  filter(time == 0.00) %>% 
  group_by(U, V, P, M, N) %>% 
  nest()



# Gather in sub df all simulations that have the same initial conditions

# Create the function to get unique scenarios indexes
list_unique_scenarios <- function(data) {
  unique_scenarios <- unique(data$scenario)
  return(unique_scenarios)
}

# Apply the function to all the data columns of the nested df
unique_scenario_values <- similar_scenarios$data %>%
  lapply(list_unique_scenarios)

# Rename the output list with proper indexes, to make "sub-scenarios" that 
# we can then compare
names(unique_scenario_values) <- paste0("sub_sc_", seq_along(unique_scenario_values))



# # Loop through each scenario and create separate dataframes
# for (i in seq_along(unique_scenario_values)) {
#   scenario <- unique_scenario_values[[i]]
#   
#   # Filter data from merged_df_full based on the scenario
#   filtered_data <- merged_df_full %>%
#     filter(scenario %in% scenario)
#   
#   # Create a dataframe with the filtered data
#   df_name <- paste0("sub_sc_", i)
#   assign(df_name, filtered_data)
#   
# }


# Loop through each sub-scenario
for (i in seq_along(unique_scenario_values)) {
  scenario_indices <- unique_scenario_values[[i]]
  
  # Filter merged_df_full based on the scenario indices
  scenario_df <- merged_df_full %>%
    filter(scenario %in% scenario_indices)
  
  # Create a dataframe with the filtered data and assign it to the name
  df_name <- paste0("sub_sc_", i)
  assign(df_name, scenario_df)
}


# eert <- sub_sc_1

# Simple graphs, with only initial and final values, for different scenarios
# of a same batch of initial values


# Get a list of variable names starting with "sub_sc"
sub_sc_dataframes <- ls(pattern = "^sub_sc")
# sub_sc_dataframes <- ls(pattern = "^eert")


# Loop through each dataframe and create the plot
for (df_name in sub_sc_dataframes) {
  df <- get(df_name)
  
  processed_df <- df %>%
    pivot_longer(cols = c(V, U, P, N, M, C),
                 names_to = "species",
                 values_to = "density") %>%
    select(time, species, density, PP, scenario) %>% 
    mutate(time = as.numeric(time),
           density = as.numeric(density),
           PP = as.numeric(PP))
  
  plot <- processed_df %>%
    ggplot(aes(x = time,
               y = density,
               color = factor(scenario))) +
    geom_line() +
    facet_grid(species ~ PP,
               scales = "free") +
    labs(title = "Species density over time, for gradient PP values")
  
  # Define the file path
  file_path <- paste0("C:/Users/lab/Documents/Automation_Primary_productivity/Plots/deer_comparison/", df_name, "_plot.png")
  
  # Save the plot as an image file
  ggsave(plot, filename = file_path)
  
  # print(paste("Plot saved for", df_name))
}




sub_sc_1_long <- sub_sc_1 %>%
  pivot_longer(cols = c(V,U,P,N,M,C),
               names_to = "species",
               values_to = "density")


sub_sc_1_long %>% 
  select(time, species, density, PP) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(species)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")





sub_sc_1_long %>% 
  filter(time == 0.00 | time == 800) %>% 
  # select(time, species, density, PP) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(scenario)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")





# --> Same intercept/species: LMM with random slopes?
# 
# library(lmerTest)
# 
# 
# random_slope_model <- lmer(density ~ time+(1 + time |species),
#                            REML = FALSE,
#                            data = sub_sc_1_long)
# 
# summary(random_slope_model)




# Assuming `unique_scenario_values` is your list of unique scenario values
# unique_scenario_values <- ...  # List of unique scenario values

test <- unique_scenario_values[[1]]

# Loop through each scenario and process the data
for (i in seq_along(test)) {
  scenario_df <- get(paste0("sub_sc_", i))
  
  processed_df <- scenario_df %>%
    pivot_longer(cols = c(V, U, P, N, M, C),
                 names_to = "species",
                 values_to = "density")
  
  processed_df <- processed_df %>%
    select(time, species, density, PP) %>% 
    mutate(time = as.numeric(time),
           density = as.numeric(density),
           PP = as.numeric(PP))
  
  plot <- processed_df %>%
    ggplot(aes(x = time,
               y = density,
               color = factor(species))) +
    geom_line() +
    facet_grid(species ~ PP,
               scales = "free") +
    labs(title = "Species density over time, for gradient PP values")
  
  # Define the file path
  # file_path <- paste0("C:/Users/lab/Documents/Automation_Primary_productivity/Plots/deer_comparison/sub_sc_", i, ".png")
  
  # Save the plot as an image file
  # ggsave(plot, filename = file_path)
  
  # print(paste("Plot saved for sub_sc_", i))
}




# ==============================================================================
# TO be continued
# Add a growth rate

tyui <- sub_sc_1 %>% 
  mutate(growth_rate_N = N - lag(N))

tyui %>% 
  ggplot(aes(x = time,
             y = growth_rate_N,
             color= factor(scenario)))+
  geom_point()

# Overall, which scenario has the max growth rate and the max mean density of caribou?

test <- merged_df %>% 
  group_by(scenario, PP) %>% 
  mutate(growth_rate_N = N - lag(N),
         diff_time = time - lag(time),
         Rate_percent = (growth_rate_N / diff_time)/N * 100) # growth rate in percent))


test %>% 
  mutate(proies_tot = M+C) %>% 
  ggplot(aes (x = proies_tot,
              y = Rate_percent, 
              color = P))+
  geom_point()
# ===============================================================================


# Which scenarios are the furthest from what is expected with Messier 1994?


# DONE - see the "match_messier1994" script


# ================================================================================

# Quick test for functional response


write.csv(merged_df_full, "C:/Users/lab/Documents/Automation_Primary_productivity/res_simulations2/full_merged_df.csv", row.names=FALSE)




# Define the folder path
folder_path <- "~/Automation_Primary_productivity/res_simulations2"

# List files in the folder
file_names <- list.files(folder_path, full.names = TRUE)

# Select files that start with "all_simulations_scenario"
selected_files <- file_names[grep("all_simulations_scenario", file_names)]

# Read RDS files and store them in a list
data_list <- lapply(selected_files, readRDS)

# Filter out tibble elements from the list
tibble_elements <- data_list %>% 
  keep(is_tibble)


# Convert each item in data_list to a data frame and assign names
for (i in seq_along(tibble_elements)) {
  df_name <- paste0("sc", i)
  assign(df_name, as.data.frame(tibble_elements[[i]]))
}


# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("^sc", all_objects)]

# Loop through selected data frames and apply the actions
for (i in seq_along(selected_objects)) {
  obj_name <- selected_objects[i]
  df <- get(obj_name)
  results <- df %>% 
    pull(outputs) %>% 
    map_dfr(as.data.frame) %>% 
    mutate_all(as.numeric) %>% 
    group_by(PP) %>% 
    # filter(time == 0.00 | time == 800.00) %>%
    select(1:10, PP, rfonc_P_Na, rfonc_P_Nj) %>% 
    mutate(N = Na + Nj, M = Ma + Mj, C = Ca + Cj) %>% 
    select(-c(4:7, "Ca", "Cj")) %>%
    mutate(scenario = i)
  
  assign(paste0("results_", obj_name), results)
}


# Merge all scenarios in one df
# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("^results_sc", all_objects)]

# Merge the selected data frames
rfonc_df_full <- do.call(rbind, lapply(selected_objects, get))


sites <- rfonc_df_full %>% 
  filter(time == 0) %>% 
  mutate(site = ifelse(time == 0 & C == 0, "east", "west"))
  

sites_test <- rfonc_df_full
qwe <- full_join(sites, sites_test)


qwe <- qwe %>% 
  group_by(PP, scenario) %>% 
  fill(site, .direction = "down")


qwert <- qwe %>%   
  filter(time == 800) %>% 
  mutate(proies_tot = M + C,
         rep_fonc_N = rfonc_P_Na+rfonc_P_Nj)


qwert %>% 
  filter(site == "east") %>% 
  mutate(masse_proies = (M * 400 + C *60)) %>% 
  ggplot(aes(x = masse_proies,
             y = rep_fonc_N,
             color = factor(round(M,1))))+
  geom_point()

east <- qwert %>% 
  filter(site == "east", 
         M != 0)

max_row_east <- east[east$rep_fonc_N == max(east$rep_fonc_N), ]



qwert %>% 
  filter(site == "west") %>% 
  mutate(masse_proies = (M * 400 + C *60)) %>% 
  ggplot(aes(x = masse_proies,
             y = rep_fonc_N,
             color = factor(round(M,1))))+
  geom_point()

qwert

west <- qwert %>% 
  filter(site == "west", 
         M != 0)

max_row_west <- west[west$rep_fonc_N == max(west$rep_fonc_N), ]



# For a same value of fonctional response, the total prey density is different in 
# west and east

qwert %>% 
  ggplot(aes(x = round(rep_fonc_N, 2),
             y = proies_tot,
             color = factor(site)))+
  geom_point()


# For each site, what is mean value of the total prey, at each 0.2 rfonc 

qwert %>% 
  mutate(rounded_rpf = round(rep_fonc_N,2)) %>% 
  group_by(rounded_rpf) %>% 
  nest() %>% 
  pull(data) %>% 
  map_dfr(as.data.frame) %>% 
  select(rounded_rpf, site, proies_tot)



# ==============================================================================
growth_rates_test <- full_merged_df %>% 
  filter( time == 0 | time == 800)
