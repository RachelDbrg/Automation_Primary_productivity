library(readr)
library(tidyverse)
library(viridis)

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")


# == EASTERN SYSTEM ============================================================
# ==============================================================================


# == Load and tidy the data ====================================================



# # Load the df containing all the simulations for 800 years
average_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/average_density.R")
extra_low_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/extra_low_density.R")
low_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low_density.R")
high_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/high_density.R")
extra_high_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/extra_high_density.R")



# Unnest the result columns
average_density <- average_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(sc = "average")

# Unnest the result columns
low_density <- low_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(sc = "low")

# Unnest the result columns
extra_low_density <- extra_low_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(sc = "extra_low")

# Unnest the result columns
high_density <- high_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(sc = "high")

# Unnest the result columns
extra_high_density <- extra_high_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(sc = "extra_high")


# Select the final densities (ie equilibrium point)

fd_extra_low <-  extra_low_density %>% 
  filter(time == 2000)

fd_low <-  low_density %>% 
  filter(time == 2000)

fd_average <-  average_density %>% 
  filter(time == 2000)

fd_high <-  high_density %>% 
  filter(time == 2000)

fd_extra_high <-  extra_high_density %>% 
  filter(time == 2000)


# Concatenate all the df that starts with the regex "fd"
df_all_scenarios <- do.call("rbind",
                            mget(ls(pattern = "^fd")))

df_all_scenarios %>% 
  group_by(sc, PP) %>% 
  select(sc, k_U)

# Time series
extra_high_density %>%
  mutate_all(as.numeric) %>%
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>%
  ggplot( aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_grid(species ~ PP, scales = "free")


sc_order <- c("extra_low", "low", "average", "high", "extra_high")
df_all_scenarios$sc <- factor(df_all_scenarios$sc, levels = sc_order)

# Look at the final values for the different deciduous values
df_all_scenarios %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_grid( species ~ sc, scales = "free")


df_all_scenarios %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_wrap( ~species  , scales = "free")
  


# What's the relationship between productivity and carrying capacity for 
# deciduous
df_all_scenarios %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj)) %>%
  ggplot(aes(x = PP,
             y = ratio_feuillus, color = orignal)) +
  geom_point()+
  facet_grid(~sc, scales = "free")
  
  
  
df_all_scenarios %>% 
  ggplot(aes(x = PP,
             y = U,
             color = sc))+
  geom_point()+
  facet_wrap(~sc, scales = "free")

df_all_scenarios %>% 
  filter(sc == "extra_low") %>% 
  select(U)


# Créer une liste pour stocker les résultats de chaque régression
regression_results <- list()


# Parcourir les scénarios
for (scenario in unique(df_all_scenarios$sc)) {
  # Sous-ensemble des données pour le scénario en cours
  subset_data <- subset(df_all_scenarios, sc == scenario)
  
  # Régression linéaire pour le scénario en cours
  lm_model <- lm(U ~ PP, data = subset_data)
  
  # Stocker les résultats dans la liste
  regression_results[[paste("Scenario", scenario)]] <- lm_model
}

# Accéder aux résultats des régressions pour chaque scénario
for (scenario_name in names(regression_results)) {
  cat("Régression pour", scenario_name, ":\n")
  print(summary(regression_results[[scenario_name]]))
}


# == Moose and deciduous vegetation ============================================
# Look at moose functional response on vegetation
# And total response?

# Functional response
df_all_scenarios %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj)) %>%
  ggplot(aes(x = PP,
             y = rep_fonc_MU,
             color = orignal))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of moose on deciduous (kg/year)")


# Numerical response
df_all_scenarios %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         P = as.numeric(P)) %>%
  ggplot(aes(x = PP,
             y = rep_totale_MU,
             color = P))+
  geom_point()+
  # scale_color_viridis() +
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of moose on deciduous (kg/year)")+
  theme_minimal()



# == Wolf functional response on moose =========================================

# Functional response
df_all_scenarios %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         rep_fonc_loup_orignal = as.numeric(rfonc_P_Ma+rfonc_P_Mj)) %>%
  ggplot(aes(x = orignal,
             y = rep_fonc_loup_orignal,
             color = as.numeric(PP)))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of wolf on moose (ind/year)")


# Numerical response on moose
df_all_scenarios %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         rep_fonc_loup_orignal = as.numeric(rfonc_P_Ma+rfonc_P_Mj),
         P= as.numeric(P)) %>%
  ggplot(aes(x = PP,
             y = rep_fonc_loup_orignal * P,
             color = as.numeric(P)))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of wolf on moose (prey eaten by the whole wolf population, in ind/year)")


# == Wolf functional response on caribou =======================================

# Functional response on caribou
df_all_scenarios %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         rep_fonc_loup_orignal = as.numeric(rfonc_P_Ma+rfonc_P_Mj),
         rep_fonc_loup_caribou = as.numeric(rfonc_P_Na+rfonc_P_Nj)) %>%
  ggplot(aes(x = orignal,
             y = rep_fonc_loup_caribou,
             color = as.numeric(PP)))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of wolf on caribou (ind/year)")


# Numerical response on caribou
df_all_scenarios %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         rep_fonc_loup_orignal = as.numeric(rfonc_P_Ma+rfonc_P_Mj),
         rep_fonc_loup_caribou = as.numeric(rfonc_P_Na+rfonc_P_Nj),
         P= as.numeric(P)) %>%
  ggplot(aes(x = orignal,
             y = rep_fonc_loup_caribou * P,
             color = as.numeric(P)))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of wolf on caribou (prey eaten by the whole wolf population, in ind/year)")



# ==============================================================================
# == WESTERN SYSTEM ============================================================
# ==============================================================================

# Directory where your RDS files are located
directory <- "~/Automation_Primary_Productivity/Varying_prey_density/"

# List RDS files in the directory that end with "_west"
west_files <- list.files(path = directory, pattern = "_west\\.R$", full.names = TRUE)

# Create a list to store the data
west_data_list <- list()

# Loop through each file and read it into R
for (file_path in west_files) {
  # Extract the name without the directory and extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the RDS file and store it in the list
  west_data_list[[file_name]] <- readRDS(file_path)
  
  list2env(west_data_list, envir = .GlobalEnv)
}


# Get the names of data frames ending with "_west"
west_data_frame_names <- ls(pattern = "_west$")

# Create a list to store the modified data frames
modified_data_frames <- list()

# Loop through each data frame and apply the operations
for (data_frame_name in west_data_frame_names) {
  # Get the data frame by its name
  data_frame <- get(data_frame_name)
  # print(data_frame_name)
  
  # Apply the operations
  modified_data_frame <- data_frame %>%
    pull(outputs) %>%
    map_dfr(as.data.frame) %>% 
    mutate(sc = str_sub(data_frame_name, end = -14))
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}

# Access the modified data for a specific data frame, e.g., "average_density_west"
average_density_west <- modified_data_frames[["average_density_west"]]
extra_low_density_west <- modified_data_frames[["extra_low_density_west"]]
low_density_west <- modified_data_frames[["low_density_west"]]
high_density_west <- modified_data_frames[["high_density_west"]]
extra_high_density_west <- modified_data_frames[["extra_high_density_west"]]


# 
# # Load the df containing all the simulations for 2000 years
# average_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/average_density_west.R")
# extra_low_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/extra_low_density_west.R")
# low_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low_density_west.R")
# high_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/high_density_west.R")
# extra_high_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/extra_high_density_west.R")

# 
# # Unnest the result columns
# average_density_west <- average_density_west %>% 
#   pull (outputs) %>% 
#   map_dfr(as.data.frame) %>% 
#   mutate(sc = "average")
# 
# # Unnest the result columns
# low_density_west <- low_density_west %>% 
#   pull (outputs) %>% 
#   map_dfr(as.data.frame) %>% 
#   mutate(sc = "low")
# 
# # Unnest the result columns
# extra_low_density_west <- extra_low_density_west %>% 
#   pull (outputs) %>% 
#   map_dfr(as.data.frame) %>% 
#   mutate(sc = "extra_low")
# 
# # Unnest the result columns
# high_density_west <- high_density_west %>% 
#   pull (outputs) %>% 
#   map_dfr(as.data.frame) %>% 
#   mutate(sc = "high")
# 
# # Unnest the result columns
# extra_high_density_west <- extra_high_density_west %>% 
#   pull (outputs) %>% 
#   map_dfr(as.data.frame) %>% 
#   mutate(sc = "extra_high")


# =========Look at the final densities==========================================

# List data frame names ending with "_west"
west_data_frame_names <- ls(pattern = "_west$")

# Create a list to store the filtered data frames
filtered_data_frames <- list()

# Loop through each data frame name and apply the filter
for (df_name in west_data_frame_names) {
  df <- get(df_name)
  filtered_df <- df %>%
    filter(time == 2000)
  filtered_data_frames[[df_name]] <- filtered_df
}

# If you want to assign the filtered data frames to new objects in the global environment:
for (i in seq_along(filtered_data_frames)) {
  assign(paste0("fd_", names(filtered_data_frames)[i]), filtered_data_frames[[i]], envir = .GlobalEnv)
}


# fd_extra_low_west <-  extra_low_density_west %>% 
#   filter(time == 2000)
# 
# fd_low_west <-  low_density_west %>% 
#   filter(time == 2000)
# 
# fd_average_west <-  average_density_west %>% 
#   filter(time == 2000)
# 
# fd_high_west <-  high_density_west %>% 
#   filter(time == 2000)
# 
# fd_extra_high_west <-  extra_high_density_west %>% 
#   filter(time == 2000)


# Concatenate all the df that starts with the regex "fd" and ends with "_west"
df_all_scenarios_west <- do.call("rbind",
                                mget(ls(pattern = "^fd.*_west$")))


# Time series
extra_low_density_west %>%
  mutate_all(as.numeric) %>%
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>%
  ggplot( aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_grid(species ~ PP, scales = "free")


sc_order <- c("extra_low", "low", "average", "high", "extra_high")
df_all_scenarios_west$sc <- factor(df_all_scenarios_west$sc, levels = sc_order)

# Look at the final values for the different deciduous values
df_all_scenarios_west %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_grid( species ~ sc, scales = "free")


df_all_scenarios_west %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_wrap( ~species  , scales = "free")



# What's the relationship between productivity and carrying capacity for 
# deciduous
df_all_scenarios_west %>% 
  filter(sc == "average") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         proies_tot = orignal + cerf,
         P = as.numeric(P)) %>%
  ggplot(aes(x = PP,
             y = ratio_feuillus, 
             color = P)) +
  geom_point()+
  facet_grid(~sc, scales = "free")



df_all_scenarios_west %>% 
  ggplot(aes(x = PP,
             y = U,
             color = sc))+
  geom_point()+
  facet_wrap(~sc, scales = "free")

df_all_scenarios %>% 
  filter(sc == "extra_low") %>% 
  select(U)


# Créer une liste pour stocker les résultats de chaque régression
regression_results <- list()


# Parcourir les scénarios
for (scenario in unique(df_all_scenarios_west$sc)) {
  # Sous-ensemble des données pour le scénario en cours
  subset_data <- subset(df_all_scenarios_west, sc == scenario)
  
  # Régression linéaire pour le scénario en cours
  lm_model <- lm(U ~ PP, data = subset_data)
  
  # Stocker les résultats dans la liste
  regression_results[[paste("Scenario", scenario)]] <- lm_model
}

# Accéder aux résultats des régressions pour chaque scénario
for (scenario_name in names(regression_results)) {
  cat("Régression pour", scenario_name, ":\n")
  print(summary(regression_results[[scenario_name]]))
}


# == Moose and deciduous vegetation ============================================
# Look at moose functional response on vegetation
# And total response?

# Functional response
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj)) %>%
  ggplot(aes(x = PP,
             y = rep_fonc_MU,
             color = orignal))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of moose on deciduous (kg/year)")


# Numerical response
df_all_scenarios %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         P = as.numeric(P)) %>%
  ggplot(aes(x = PP,
             y = rep_totale_MU,
             color = P))+
  geom_point()+
  # scale_color_viridis() +
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of moose on deciduous (kg/year)")+
  theme_minimal()


# == Deer and deciduous vegetation ============================================
# Look at moose functional response on vegetation
# And total response?

# Functional response
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj)) %>%
  ggplot(aes(x = PP,
             y = rep_fonc_MU,
             color = cerf))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of moose on deciduous (kg/year)")


# Numerical response
df_all_scenarios %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         P = as.numeric(P)) %>%
  ggplot(aes(x = PP,
             y = rep_totale_MU,
             color = P))+
  geom_point()+
  # scale_color_viridis() +
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of moose on deciduous (kg/year)")+
  theme_minimal()
