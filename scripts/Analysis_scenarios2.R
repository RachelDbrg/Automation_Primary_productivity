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

# qw <- sc1 %>% 
#   pull(outputs) %>% 
#   map_dfr(as.data.frame) %>% 
#   mutate_all(as.numeric) %>% 
#   group_by(PP) %>% 
#   filter(time == 0.00 | time == 800.00) %>%
#   select(1:10, PP) %>% 
#   mutate(N_init = Na + Nj, M_init = Ma + Mj, C_init = Ca + Cj) %>% 
#   select(-c(4:6, "Ca", "Cj")) %>%
#   mutate(scenario = i)



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
