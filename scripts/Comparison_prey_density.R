library(readr)
library(tidyverse)

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")


# ==Simulations on the first 800 years==========================================
# ==============================================================================

# Load the df containing all the simulations for 800 years
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


# =========Look at the final densities==========================================

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
