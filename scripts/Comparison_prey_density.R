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
  mutate_all(as.numeric) %>% 
  mutate(sc = "average")

# Unnest the result columns
low_density <- low_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "low")

# Unnest the result columns
extra_low_density <- extra_low_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "extra_low")

# Unnest the result columns
high_density <- high_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "high")

# Unnest the result columns
extra_high_density <- extra_high_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
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
df_all_scenarios_est <- do.call("rbind",
                            mget(ls(pattern = "^fd")))

# # Select only the interesting columns (19)
# df_all_scenarios_est <- df_all_scenarios_est %>% 
#   select(c(1:10, 18:24, PP, sc))



df_all_scenarios_est %>% 
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


df_all_scenarios %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         rep_fonc_loup_orignal = as.numeric(rfonc_P_Ma+rfonc_P_Mj),
         P= as.numeric(P)) %>%
  ggplot(aes(x = orignal,
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
             color = as.numeric(PP)))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of wolf on caribou (prey eaten by the whole wolf population, in ind/year)")



# ------------------------------------------------------------------------------
# 1. PP and final densities

fd_average %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>%
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap(~species, scales = "free")
  

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
# Load the df containing all the simulations for 2000 years
average_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/average_density_west.R")
extra_low_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/extra_low_density_west.R")
low_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low_density_west.R")
high_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/high_density_west.R")
extra_high_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/extra_high_density_west.R")

# 
# # Unnest the result columns
average_density_west <- average_density_west %>%
  pull (outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate(sc = "average")

# Unnest the result columns
low_density_west <- low_density_west %>%
  pull (outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate(sc = "low")

# Unnest the result columns
extra_low_density_west <- extra_low_density_west %>%
  pull (outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate(sc = "extra_low")

# Unnest the result columns
high_density_west <- high_density_west %>%
  pull (outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate(sc = "high")

# Unnest the result columns
extra_high_density_west <- extra_high_density_west %>%
  pull (outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate(sc = "extra_high")


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


fd_extra_low_west <-  extra_low_density_west %>%
  filter(time == 2000)

fd_low_west <-  low_density_west %>%
  filter(time == 2000)

fd_average_west <-  average_density_west %>%
  filter(time == 2000)

fd_high_west <-  high_density_west %>%
  filter(time == 2000)

fd_extra_high_west <-  extra_high_density_west %>%
  filter(time == 2000)


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
df_all_scenarios_west %>% 
  # filter(sc == "extra_high") %>% 
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
# Look at deer functional response on vegetation
# And total response?

# Functional response
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj)) %>%
  ggplot(aes(x = PP,
             y = rep_fonc_CU,
             color = cerf))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of deer on deciduous (kg/year)")


# Numerical response
df_all_scenarios_west %>% 
  # filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         P = as.numeric(P),
         cerf = as.numeric(Ca+Cj)) %>%
  ggplot(aes(x = PP,
             y = rep_totale_CU,
             color = P))+
  geom_point()+
  # scale_color_viridis() +
  facet_wrap(~sc, scales = "free")+
  labs (y = "Numerical response of deer on deciduous (kg/year)")+
  theme_minimal()


# == Wolf response on moose ==========================================
# Functional
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncM = rfonc_P_Mj+rfonc_P_Ma,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj)) %>%
  ggplot(aes(x = PP,
             y = rfoncM,
             color = proies_tot))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of wolf on moose (ind/year)")


df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncM = rfonc_P_Mj+rfonc_P_Ma,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP)) %>%
  ggplot(aes(x = orignal,
             y = rfoncM,
             color = PP))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of wolf on moose (ind/year)")


# Numerical response
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncM = rfonc_P_Mj+rfonc_P_Ma,
         rfonc_totM = rfoncM * P,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP)) %>%
  ggplot(aes(x = orignal,
             y = rfonc_totM,
             color = PP))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Total response of wolf on caribou (ind/year)")

# Zoom on the high dnesity scenario
df_all_scenarios_west %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         P = as.numeric(P),
         rfoncM = rfonc_P_Mj+rfonc_P_Ma,
         rfonc_totM = rfoncM * P,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP)) %>%
  ggplot(aes(x = orignal,
             y = rfonc_totM,
             color = P))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Total response of wolf on caribou (ind/year)")



# == Wolf response on deer ==========================================
# Functional
df_all_scenarios_west %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncC = rfonc_P_Cj+rfonc_P_Ca,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj)) %>%
  ggplot(aes(x = PP,
             y = rfoncC,
             color = orignal))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of wolf on deer (ind/year)")


df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncM = rfonc_P_Mj+rfonc_P_Ma,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP),
         rfoncC = rfonc_P_Cj+rfonc_P_Ca) %>%
  ggplot(aes(x = orignal,
             y = rfoncC,
             color = PP))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of wolf on moose (ind/year)")

  
# Numerical response
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncC = rfonc_P_Cj+rfonc_P_Ca,
         rfonc_totC = rfoncC * P,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP),
         P = as.numeric(P),
         rfoncM = as.numeric(rfonc_P_Mj+rfonc_P_Ma)) %>%
  ggplot(aes(x = orignal,
             y = rfonc_totC,
             color = rfoncM))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Total response of wolf on deer (ind/year)")


# Zoom on the high dnesity scenario
df_all_scenarios_west %>% 
  filter(sc == "extra_high") %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         P = as.numeric(P),
         rfoncM = rfonc_P_Mj+rfonc_P_Ma,
         rfonc_totM = rfoncM * P,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP)) %>%
  ggplot(aes(x = orignal,
             y = rfonc_totM,
             color = P))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Total response of wolf on caribou (ind/year)")




# == Wolf functional response on caribou ==========================================
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncN = as.numeric(rfonc_P_Nj+rfonc_P_Na),
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP),
         P = as.numeric(P)) %>%
  ggplot(aes(x = orignal,
             y = rfoncN,
             color = PP))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Fonctional response (per capita) of deer on deciduous (kg/year)")


# Numerical response
df_all_scenarios_west %>% 
  mutate(ratio_feuillus = U/k_U,
         orignal = as.numeric(Ma+Mj),
         cerf = as.numeric(Ca+Cj),
         rfoncN = as.numeric(rfonc_P_Nj+rfonc_P_Na),
         rfonc_totN = rfoncN * P,
         proies_tot = as.numeric(Ma + Mj + Ca + Cj),
         PP = as.numeric(PP)) %>%
  ggplot(aes(x = orignal,
             y = rfonc_totN,
             color = PP))+
  geom_point()+
  facet_wrap(~sc, scales = "free")+
  labs (y = "Total response of wolf on caribou (ind/year)")


# ------------------------------------------------------------------------------
# 1. PP and final densities

fd_average_density_west %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>%
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap(~species, scales = "free")



# == Comparison of the final densities with and wo deer ========================

# Wo deer
long_fd_average <- fd_average %>% 
  select(PP, c(2:10)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  mutate(zone = "est")

# With deer
long_fd_average_west <- fd_average_density_west %>% 
  select(PP, c(2:10)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  mutate(zone = "ouest")



merged_est_west <- bind_rows(long_fd_average,
                             long_fd_average_west)

merged_est_west %>% 
  ggplot(aes(x = PP))+
  geom_point(aes(y = density, color = zone))+
  facet_wrap(~species, scales = "free")


# ------------------------------------------------------------------------------
# 1. PP and mean densities

# Wo deer
average_density %>% 
  select(PP, c(2:10)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  group_by(species, PP) %>% 
  summarise(meandensity = mean(density)) %>% 
    ggplot(aes(x = PP,
               y = meandensity))+
    geom_point()+
    facet_wrap(~species, scales = "free")


# With deer
average_density_west %>% 
  select(PP, c(2:10)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  group_by(species, PP) %>% 
  summarise(meandensity = mean(density)) %>% 
  ggplot(aes(x = PP,
             y = meandensity))+
  geom_point()+
  facet_wrap(~species, scales = "free")


# 1. Comparison of final and average densities

# With deer
# Final
long_final_densities <- fd_average_density_west %>% 
  select(PP, c(2:10)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>%
  select(c(PP, species, density)) %>% 
  mutate(type = "final")

# Mean
long_mean_densities <- average_density_west %>% 
  select(PP, c(2:10)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  group_by(species, PP) %>% 
  summarise(density = mean(density)) %>% 
  mutate(type = "mean")

comparison_final_mean_west <- bind_rows(long_final_densities, long_mean_densities)

comparison_final_mean_west %>% 
  ggplot(aes(x = PP,
             y = density,
             color = type))+
  geom_point()+
  facet_wrap(~species, scales = "free")



# ------------------------------------------------------------------------------
# 1. PP and cycles amplitude

# Est
average_density %>% 
  select(c(1, 4:8), PP) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:5),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(PP)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")


# 1. PP and cycles amplitude
# Ouest
average_density_west %>% 
  select(c(1, 4:10), PP) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:5, 7,8),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(PP)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")


# ------------------------------------------------------------------------------
# 1. PP and growth rate

# Plot growth rate for every PP value

# East
max_growth_rate <- average_density %>% 
  select(-sc) %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  mutate(growth_rate_N = Na - lag(Na),
         growth_rate_M = Ma - lag(Ma),
         growth_rate_P = P - lag(P),
         growth_rate_C = Ca - lag(Ca),
         diff_time = time - lag(time),
         Rate_percent_N = (growth_rate_N / diff_time)/Na * 100,
         Rate_percent_M = (growth_rate_M / diff_time)/Ma * 100,
         Rate_percent_P = (growth_rate_P / diff_time)/P * 100,
         Rate_percent_C = (growth_rate_C / diff_time)/Ca * 100,
         proies_tot = Ma + Na) %>% 
  filter(PP == 0 | PP == 1)

# Get the max values
test_growth_rate <- max_growth_rate %>% 
  filter(PP == 1,
         P <= 0.03) %>% 
  select(1:10, 54:57)

max <- which.max(test_growth_rate$Rate_percent_P)

test_growth_rate[max,]

# Get the max growth rate values for each species, for every PP value
max_values_df <- max_growth_rate %>%
  select(-Rate_percent_C) %>% 
  group_by(PP) %>%
  summarize(across(starts_with("Rate_percent"), ~ max(., na.rm = TRUE))) %>% 
  mutate(zone = "est",
         Rate_percent_C = 0)


# West
max_growth_rate_west <- average_density_west %>% 
  select(-sc) %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  mutate(growth_rate_N = Na - lag(Na),
         growth_rate_M = Ma - lag(Ma),
         growth_rate_P = P - lag(P),
         growth_rate_C = Ca - lag(Ca),
         diff_time = time - lag(time),
         Rate_percent_N = (growth_rate_N / diff_time)/Na * 100,
         Rate_percent_M = (growth_rate_M / diff_time)/Ma * 100,
         Rate_percent_P = (growth_rate_P / diff_time)/P * 100,
         Rate_percent_C = (growth_rate_C / diff_time)/Ca * 100,
         proies_tot = Ma + Na) %>% 
  filter(PP == 0 | PP == 1)

# Get the max values
test_growth_rate_west <- max_growth_rate_west %>% 
  filter(PP == 1,
         P <= 0.03) %>% 
  select(1:10, 57:60)

max <- which.max(test_growth_rate_west$Rate_percent_C)

test_growth_rate_west[max,]


# Get the max growth rate values for each species, for every PP value
max_values_df_west <- max_growth_rate_west %>%
  group_by(PP) %>%
  summarize(across(starts_with("Rate_percent"), ~ max(., na.rm = TRUE))) %>% 
  mutate(zone = "ouest")

max_values_df_west %>% 
  ggplot(aes(x = Rate_percent_M,
             y = Rate_percent_P))+
  geom_point()

# Comparison of eastern and western sites
max_growth_rates <- bind_rows(max_values_df,
                              max_values_df_west)


max_growth_rates %>% 
  pivot_longer(c(2:4,6),
               names_to = "species",
               values_to = "growth_rate") %>% 
  ggplot(aes(x = PP,
             y = growth_rate,
             color = zone))+
  geom_point()+
  facet_wrap(~species, scales = "free")+
  labs( y = "max growth rate of species")


# == 3. ========================================================================

average_density %>% 
  filter(time == 2000) %>% 
  ggplot(aes(x = as.numeric(PP),
             y = as.numeric(rfonc_P_Na),
             color = as.numeric(Ma)))+
  geom_point()



# Test avec ratio de caribous manges/total de proie
df_all_scenarios %>% 
  filter(time == 2000) %>% 
  mutate(ratio = as.numeric(rfonc_P_Na/proies_tot),
         orignaux_manges = as.numeric(rfonc_P_Ma*P),
         caribous_manges = as.numeric(rfonc_P_Na*P),
         diet_loup = caribous_manges+orignaux_manges,
         ratio_1 = caribous_manges/(caribous_manges+orignaux_manges),
         ratio_2 = orignaux_manges/(caribous_manges+orignaux_manges)) %>% 
  ggplot(aes(x = as.numeric(PP)))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  facet_wrap(~sc)+
  labs(y = "Ratio de proie mangee",
       x = "PP")


# Test avec ratio de caribous manges/total de proie
df_all_scenarios_west %>% 
  filter(time == 2000) %>% 
  mutate(ratio = as.numeric(rfonc_P_Na/proies_tot),
         orignaux_manges = as.numeric(rfonc_P_Ma*P),
         caribous_manges = as.numeric(rfonc_P_Na*P),
         cerfs_manges = as.numeric(rfonc_P_Ca*P),
         diet_loup = caribous_manges+orignaux_manges+cerfs_manges,
         ratio_1 = caribous_manges/(diet_loup),
         ratio_2 = orignaux_manges/(diet_loup),
         ratio_3 = cerfs_manges/diet_loup) %>% 
  ggplot(aes(x = as.numeric(PP)))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  facet_wrap(~sc)+
  labs(y = "Ratio de proie mangee",
       x = "PP")


# Comparison of the wolf diet for both scenarios
# Est
wolf_diet <- df_all_scenarios %>% 
  filter(time == 2000) %>% 
  mutate(ratio = as.numeric(rfonc_P_Na/proies_tot),
         orignaux_manges = as.numeric(rfonc_P_Ma*P),
         caribous_manges = as.numeric(rfonc_P_Na*P),
         cerfs_manges = as.numeric(rfonc_P_Ca*P),
         diet_loup = caribous_manges+orignaux_manges,
         ratio_1 = caribous_manges/(caribous_manges+orignaux_manges),
         ratio_2 = orignaux_manges/(caribous_manges+orignaux_manges),
         ratio_3 = cerfs_manges/diet_loup) %>% 
  select(c(PP,1:10,52:58)) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est")


# West
wolf_diet_west <- df_all_scenarios_west %>% 
  filter(time == 2000) %>% 
  mutate(ratio = as.numeric(rfonc_P_Na/proies_tot),
         orignaux_manges = as.numeric(rfonc_P_Ma*P),
         caribous_manges = as.numeric(rfonc_P_Na*P),
         cerfs_manges = as.numeric(rfonc_P_Ca*P),
         diet_loup = caribous_manges+orignaux_manges+cerfs_manges,
         ratio_1 = caribous_manges/(diet_loup),
         ratio_2 = orignaux_manges/(diet_loup),
         ratio_3 = cerfs_manges/diet_loup) %>% 
  select(c(PP,1:10,54:60)) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "ouest")


wolf_diet_merged <- bind_rows(wolf_diet,
                              wolf_diet_west)


# Ratio de proie mangee en fct de la densite d'orignal
wolf_diet_merged %>% 
  ungroup() %>% 
  ggplot(aes(x = Ma))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Densité d'orignal")+
  facet_grid(zone~.)


# Ratio de proie mangee en fct de la densite totale de proies (sauf caribou)
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(proie_tot = Ma+Mj+((Ca+Cj))) %>% 
  ggplot(aes(x = proie_tot))+
  geom_line(aes(y = as.numeric(ratio_1),
                color = "caribous"))+
  geom_line(aes(y = as.numeric(ratio_2),
                color = "orignaux"))+
  geom_line(aes(y = as.numeric(ratio_3),
                color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Densité de proies (hors caribou")+
  facet_grid(zone~.)


# Ratio de proie mangee en fct de la densite totale de proies (hors caribou)
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(proie_tot = Ma+Mj+((Ca+Cj)*0.2)) %>% 
  ggplot(aes(x = proie_tot))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Densité d'orignal")+
  facet_grid(zone~.)



# See with the weigth
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(prey_biomass_tot = ((Ma+Mj)*350)+((Ca+Cj)*70)+((Na+Nj)*100)) %>% 
  ggplot(aes(x = prey_biomass_tot))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Available prey biomass")+
  facet_grid(zone~.)


# See with the PP
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(prey_biomass_tot = ((Ma+Mj)*350)+((Ca+Cj)*70)+((Na+Nj)*100)) %>% 
  # filter(U <= 2e5) %>%
  ggplot(aes(x = U))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Deciduous biomass")+
  facet_grid(zone~.)


# ==============================================================================
# == ZOOM SUR LES DENSITES INTERMEDIAIRES DE FEUILLUS ==========================
library(readr)
library(tidyverse)
library(viridis)

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")


# == EASTERN SYSTEM ============================================================

low1_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low1_density.R")
low2_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low2_density.R")
low3_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low3_density.R")
low4_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low4_density.R")
low5_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low5_density.R")
low6_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low6_density.R")
low7_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low7_density.R")
low8_density <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low8_density.R")


# Unnest the result columns
low1_density <- low1_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate(sc = "low1")

# Unnest the result columns
low2_density <- low2_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low2")

# Unnest the result columns
low3_density <- low3_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low3")

# Unnest the result columns
low4_density <- low4_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low4")

# Unnest the result columns
low5_density <- low5_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low5")

# Unnest the result columns
low6_density <- low6_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low6")

# Unnest the result columns
low7_density <- low7_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low7")

# Unnest the result columns
low8_density <- low8_density %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low8")

# Select the final densities (ie equilibrium point)

add_fd_low1 <-  low1_density %>% 
  filter(time == 2000)

add_fd_low2 <-  low2_density %>% 
  filter(time == 2000)

add_fd_low3 <-  low3_density %>% 
  filter(time == 2000)

add_fd_low4 <-  low4_density %>% 
  filter(time == 2000)

add_fd_low5 <-  low5_density %>% 
  filter(time == 2000)

add_fd_low6 <-  low6_density %>% 
  filter(time == 2000)

add_fd_low7 <-  low7_density %>% 
  filter(time == 2000)

add_fd_low8 <-  low8_density %>% 
  filter(time == 2000) 



# Concatenate all the df that starts with the regex "fd"
df_all_scenarios <- do.call("rbind",
                            mget(ls(pattern = "^add_fd_low")))


df_all_scenarios_test <- df_all_scenarios_est %>%
  select(c(1:10, 18:24, PP, sc)) %>% 
  mutate(across(!sc, as.numeric))

df_all_scenarios_test1 <- df_all_scenarios %>% 
  select(c(1:10, 18:24, PP, sc)) %>% 
  mutate(across(!sc, as.numeric))

total_data_est <- bind_rows(df_all_scenarios_test,df_all_scenarios_test1)

# Comparison of the wolf diet for both scenarios
# Est

wolf_diet <- total_data_est %>% 
  filter(time == 2000) %>% 
  mutate(proies_tot= Ma+Mj+Na+Nj+Ca+Cj,
         ratio = as.numeric(rfonc_P_Na/proies_tot),
         orignaux_manges = as.numeric(rfonc_P_Ma*P),
         caribous_manges = as.numeric(rfonc_P_Na*P),
         cerfs_manges = as.numeric(rfonc_P_Ca*P),
         diet_loup = caribous_manges+orignaux_manges,
         ratio_1 = caribous_manges/(caribous_manges+orignaux_manges),
         ratio_2 = orignaux_manges/(caribous_manges+orignaux_manges),
         ratio_3 = cerfs_manges/diet_loup) %>% 
  # select(c(PP,1:10,52:59)) %>% 
  # mutate_all(as.numeric) %>% 
  mutate(zone = "est")



# == WESTERN SYSTEM ============================================================

low1_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low1_density_west.R")
low2_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low2_density_west.R")
low3_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low3_density_west.R")
low4_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low4_density_west.R")
low5_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low5_density_west.R")
low6_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low6_density_west.R")
low7_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low7_density_west.R")
low8_density_west <- readRDS("~/Automation_Primary_Productivity/varying_prey_density/low8_density_west.R")


# Unnest the result columns
low1_density_west <- low1_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low1")

# Unnest the result columns
low2_density_west <- low2_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low2")

# Unnest the result columns
low3_density_west <- low3_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low3")

# Unnest the result columns
low4_density_west <- low4_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low4")

# Unnest the result columns
low5_density_west <- low5_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low5")

# Unnest the result columns
low6_density_west <- low6_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low6")

# Unnest the result columns
low7_density_west <- low7_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low7")

# Unnest the result columns
low8_density_west <- low8_density_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)%>% 
  mutate(sc = "low8")

# Select the final densities (ie equilibrium point)

add_fd_low1_west <-  low1_density_west %>% 
  filter(time == 2000)

add_fd_low2_west <-  low2_density_west %>% 
  filter(time == 2000)

add_fd_low3_west <-  low3_density_west %>% 
  filter(time == 2000)

add_fd_low4_west <-  low4_density_west %>% 
  filter(time == 2000)

add_fd_low5_west <-  low5_density_west %>% 
  filter(time == 2000)

add_fd_low6_west <-  low6_density_west %>% 
  filter(time == 2000)

add_fd_low7_west <-  low7_density_west %>% 
  filter(time == 2000)

add_fd_low8_west <-  low8_density_west %>% 
  filter(time == 2000)



# Concatenate all the df that starts with the regex "fd" and ends with "_west"
df_all_scenarios_west_add <- do.call("rbind",
                                 mget(ls(pattern = "^add_fd.*_west$")))


total_data_west <- bind_rows(df_all_scenarios_west,df_all_scenarios_west_add)

total_data_west <- total_data_west %>% 
  select(c(1:10, 18:24, PP, sc))


# West
wolf_diet_west <- total_data_west %>% 
  # filter(time == 2000) %>% 
  mutate(proies_tot= Ma+Mj+Na+Nj+Ca+Cj,
         ratio = as.numeric(rfonc_P_Na/proies_tot),
         orignaux_manges = as.numeric(rfonc_P_Ma*P),
         caribous_manges = as.numeric(rfonc_P_Na*P),
         cerfs_manges = as.numeric(rfonc_P_Ca*P),
         diet_loup = caribous_manges+orignaux_manges+cerfs_manges,
         ratio_1 = caribous_manges/(diet_loup),
         ratio_2 = orignaux_manges/(diet_loup),
         ratio_3 = cerfs_manges/diet_loup) %>% 
  # select(c(PP,1:10,52:59)) %>% 
  mutate(across(!sc, as.numeric)) %>%
  mutate(zone = "ouest")



wolf_diet_merged <- bind_rows(wolf_diet,
                              wolf_diet_west)

saveRDS(wolf_diet_merged, file = "wolf_diet_merged.rds")


# Ratio de proie mangee en fct de la densite d'orignal
wolf_diet_merged %>% 
  ungroup() %>% 
  ggplot(aes(x = Ma))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Densité d'orignal")+
  facet_grid(zone~.)



# Ratio de proie mangee en fct de la densite totale de proies (sauf caribou)
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(proie_tot = Ma+Mj+((Ca+Cj))) %>% 
  ggplot(aes(x = proie_tot))+
  geom_point(aes(y = as.numeric(ratio_1),
                color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Densité de proies (hors caribou")+
  facet_grid(zone~.)


# Ratio de proie mangee en fct de la densite totale de proies (hors caribou)
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(proie_tot = Ma+Mj+((Ca+Cj)*0.2)) %>% 
  ggplot(aes(x = proie_tot))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Proie totale")+
  facet_grid(zone~.)



# See with the weigth
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(prey_biomass_tot = ((Ma+Mj)*350)+((Ca+Cj)*70)+((Na+Nj)*100)) %>% 
  ggplot(aes(x = prey_biomass_tot))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Available prey biomass")+
  facet_grid(zone~.)


# See with the PP
wolf_diet_merged %>% 
  ungroup() %>% 
  mutate(prey_biomass_tot = ((Ma+Mj)*350)+((Ca+Cj)*70)+((Na+Nj)*100)) %>% 
  filter(U <= 2e5) %>%
  ggplot(aes(x = U))+
  geom_point(aes(y = as.numeric(ratio_1),
                 color = "caribous"))+
  geom_point(aes(y = as.numeric(ratio_2),
                 color = "orignaux"))+
  geom_point(aes(y = as.numeric(ratio_3),
                 color = "cerfs"))+
  labs(y = "Ratio de proie mangee",
       x = "Deciduous biomass")+
  facet_grid(zone~.)

