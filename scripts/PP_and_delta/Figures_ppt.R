library(readr)
library(tidyverse)

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")


# ==Simulations on the first 800 years==========================================
# ==============================================================================

# Load the df containing all the simulations for 800 years
all_simulations <- readRDS ("all_simulations.R")
all_simulations_wo_deer <- readRDS ("all_simulations_without_deer.R")


# Unnest the result columns
unnested_west <- all_simulations %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)

unnested_est <- all_simulations_wo_deer %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)


# ==East scenario ==============================================================
# ==============================================================================

# Without deers
est_short <- unnested_est %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)

# Melt the dataframe to a long format
long_east_short <- est_short %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")

# Create a facet plot of density of all species during time
# for PP from 0 to 10
ggplot(long_east_short, aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_grid(species ~ PP, scales = "free")

final_densities_east <- long_east_short %>% 
  filter(time == 800) %>% 
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap( ~ species, scales = "free")

final_densities_east

# Get the initial values
est_short %>% 
  filter(time == 0)


# Time series
long_east_short %>% 
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

final_densities_east <- long_east_short %>% 
  filter(time == 800) 


# Time series
long_east_short %>% 
  select(time, species, density, PP) %>% 
  filter(species == "Ma") %>% 
  filter(time %in% c(700:800)) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = as.factor(PP)))+
  geom_line()+
  labs(title = "Species density over time, for gradient PP values")



# Limit cycles?
# Plot prey density against predator's

long_df_est <- long_df_est %>% 
  mutate(type = ifelse(species == "P", "predator", ifelse(species == "U" | species == "V", "vegetation", "prey")))

# Convert the df to the long format
limit_cycles <- est_short %>% 
  pivot_longer(cols = c(5:8, 10, 11),
               names_to = "species",
               values_to = "density")


limit_cycles %>% 
  filter(species != "Ca" & species != "Cj") %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(PP)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")




# =West scenario================================================================
# ==============================================================================

# Select only columns with species density
test <- subdata_unnested %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)


# Melt the dataframe to a long format
long_df_west <- test %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")

# Create a facet plot of density of all species during time
# for PP from 0 to 10
ggplot(long_df_west, aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_grid(species ~ PP, scales = "free")


final_densities_west <- long_df_west %>% 
  filter(time == 800) %>% 
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap( ~ species, scales = "free")

final_densities_west

# Get the initial values
test %>% 
  filter(time == 0)


# Time series
long_df_west %>% 
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

final_densities_east <- long_df %>% 
  filter(time == 800) 


# Time series
long_df %>% 
  select(time, species, density, PP) %>% 
  filter(species == "Ma") %>% 
  filter(time %in% c(700:800)) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = as.factor(PP)))+
  geom_line()+
  labs(title = "Species density over time, for gradient PP values")



# ===Simulations on the first 2000 years========================================
# ==============================================================================

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")

# ==East scenario ==============================================================
# ==============================================================================

# Load the df containing all the simulations 
all_simulations_wo_deer_long_period <- readRDS ("all_simulations_without_deer_long_period.R")

# Unnest the result columns
unnested_simulations_long_period <- all_simulations_wo_deer_long_period %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)


# Without deers
test_est_long_period <- unnested_simulations_long_period %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)

# Melt the dataframe to a long format
long_df_est <- test_est_long_period %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")

# Plot time series for 10 levels of PP values
ggplot(long_df_est, aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_grid(species ~ PP, scales = "free")

# Get the fianl densities (for t= 2000) and plot it against PP values
final_densities_east <- long_df_est %>% 
  filter(time == 2000) %>% 
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap( ~ species, scales = "free")

final_densities_east

long_df_est %>% 
  filter(time == 2000) %>% 
  pivot_wider(names_from = species,
              values_from = density) %>% 
  View()

# Get the initial values
test %>% 
  filter(time == 0)


# Time series
long_df %>% 
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

final_densities_east <- long_df %>% 
  filter(time == 800) 



# ==============================================================================

# Link between moose functional response and deciduous density
unnested_simulations_long_period %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  ggplot(aes(U, 
             rep_fonc_MU,
             color = time))+
  geom_point()+
  facet_wrap(~ PP)

# Link between moose functional response on deciduous and its carrying capacity

unnested_simulations_long_period %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  ggplot(aes(x = rep_fonc_MU,
             y = U))+
  geom_point()+
  facet_wrap(~ PP)


# Wolf functional response on moose
unnested_simulations_long_period %>% 
  mutate_all(as.numeric) %>% 
  mutate(proies_tot = Ma + Mj + Na + Nj,
         orignal = Ma + Mj,
         rep_fonc_orignal = rfonc_P_Ma+ rfonc_P_Mj,
         rep_tot_wolf = rep_fonc_orignal * P ) %>% 
  group_by(PP) %>% 
  ggplot(aes(x = proies_tot,
             y = rep_tot_wolf,
             color = time))+
  geom_point()+
  facet_wrap(~ PP)+
  labs(y = "quantite de caribou consomme")


unnested_simulations_long_period %>% 
  mutate_all(as.numeric) %>% 
  filter(time >= 700,
         time <= 800) %>% 
  mutate(proies_tot = Ma + Mj + Na + Nj,
         orignal = Ma + Mj,
         rep_fonc_orignal = rfonc_P_Ma+ rfonc_P_Mj,
         rep_tot_wolf = rep_fonc_orignal * P ) %>% 
  ggplot(aes(x = orignal,
             y = rep_tot_wolf,
             color = time))+
  geom_point()+
  facet_wrap(~ PP)+
  labs(y = "quantite de caribou consomme")


# Wolf functional response on caribou
unnested_simulations_long_period %>% 
  mutate_all(as.numeric) %>% 
  mutate(proies_tot = Ma + Mj + Na + Nj,
         orignal = Ma + Mj,
         rep_fonc_caribou = rfonc_P_Na+ rfonc_P_Nj) %>% 
  group_by(PP) %>% 
  ggplot(aes(x = proies_tot,
             y = rep_fonc_caribou,
             color = orignal))+
  geom_point()+
  facet_wrap(~ PP)


# ==============================================================================

# Limit cycles?
# Plot prey density against predator's

long_df_est <- long_df_est %>% 
  mutate(type = ifelse(species == "P", "predator", ifelse(species == "U" | species == "V", "vegetation", "prey")))

limit_cycles <- test_est_long_period %>% 
  pivot_longer(cols = c(5:8, 10, 11),
               names_to = "species",
               values_to = "density")


limit_cycles %>% 
  filter(species != "Ca" & species != "Cj",
         time >= 100,
         time <= 800) %>% 
  ggplot(aes(x = P, 
             y = density,
             color = time))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")


# Zoom on one of these cycle for comparison
limit_cycles <- test_est_long_period %>% 
  pivot_longer(cols = c(5:8, 10, 11),
               names_to = "species",
               values_to = "density")


# Plot moose and caribou cycles with predator for extreme PP values
limit_cycles %>% 
  filter(species =="Ma"| species == "Na",
         PP == 0 | PP == 1,
         P <= 0.03) %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(species)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")




phase_1 <- limit_cycles %>% 
  filter(species =="Ma"| species == "Na",
         PP == 0 | PP == 1,
         P <= 0.03) %>% 
  pivot_wider(names_from = "species",
              values_from = "density") %>%
  mutate(growth_rate_N = Na - lag(Na),
         growth_rate_M = Ma - lag(Ma),
         growth_rate_P = P - lag(P),
         diff_time = time - lag(time),
         Rate_percent_N = (growth_rate_N / diff_time)/Na * 100,
         Rate_percent_M = (growth_rate_M / diff_time)/Ma * 100,
         Rate_percent_P = (growth_rate_P / diff_time)/P * 100)


test_growth_rate <- phase_1 %>% 
  filter(PP == 1)

max <- which.max(test_growth_rate$Rate_percent_P)

test_growth_rate[max,]



phase_1 %>% 
  filter(PP == 1) %>% 
  mutate(proie_tot = Ma+Na) %>% 
  ggplot(aes(x = proie_tot,
             y = Rate_percent_M,
             color = Na))+
  geom_point()


phase_1 %>% 
  filter(PP == 1) %>% 
  mutate(proie_tot = Ma+Na) %>% 
  ggplot(aes(x = proie_tot,
             y = Rate_percent_N,
             color = Ma))+
  geom_point()


phase_1 %>% 
  filter(PP == 0) %>% 
  mutate(proie_tot = Ma+Na) %>% 
  ggplot(aes(x = proie_tot,
             y = Rate_percent_P,
             color = Na))+
  geom_point()


  
phase_1 <- limit_cycles %>% 
  filter(species =="Ma"| species == "Na",
         P <= 0.03) %>% 
  pivot_wider(names_from = "species",
              values_from = "density") %>%
  mutate(growth_rate_N = Na - lag(Na),
         growth_rate_M = Ma - lag(Ma),
         growth_rate_P = P - lag(P),
         diff_time = time - lag(time),
         Rate_percent_N = (growth_rate_N / diff_time)/Na * 100,
         Rate_percent_M = (growth_rate_M / diff_time)/Ma * 100,
         Rate_percent_P = (growth_rate_P / diff_time)/P * 100)


# Get the max growth rate values for each species, for every PP value
max_values_df <- phase_1 %>%
  group_by(PP) %>%
  summarize(across(starts_with("Rate_percent"), ~ max(., na.rm = TRUE)))

# Print the result
print(max_values_df)

max_values_df %>% 
  ggplot(aes(Rate_percent_M,
             Rate_percent_P))+
  geom_point()+
  labs(x = "Taux maximum de croissance de l'orignal",
       y = "Taux maximum de croissance du loup")

# Plot caribou's growth rate for every PP value

limit_cycles %>% 
  filter(species =="Ma"| species == "Na") %>% 
  pivot_wider(names_from = "species",
              values_from = "density") %>%
  mutate(growth_rate_N = Na - lag(Na),
         growth_rate_M = Ma - lag(Ma),
         growth_rate_P = P - lag(P),
         diff_time = time - lag(time),
         Rate_percent_N = (growth_rate_N / diff_time)/Na * 100,
         Rate_percent_M = (growth_rate_M / diff_time)/Ma * 100,
         Rate_percent_P = (growth_rate_P / diff_time)/P * 100,
         proies_tot = Ma + Na) %>% 
  ggplot(aes(x = proies_tot,
             y = Rate_percent_N))+
  geom_line()+
  facet_wrap(~ PP)

# Get the means values of the cycles

long_df_est %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density)) %>% 
  ggplot(aes(x = PP, 
             y = mean_density))+
  geom_point()+
  facet_wrap(~ species, 
             scales = "free")

long_df_est %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density)) %>% 
  pivot_wider(names_from = species,
              values_from = mean_density) %>% View()


# Zoom on the time steps between 100 and 800
limit_cycles %>% 
  filter(species != "Ca" & species != "Cj",
         time >= 100,
         time <= 800) %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density)) %>% 
  ggplot(aes(x = PP, 
             y = mean_density))+
  geom_point()+
  facet_wrap(~ species, 
             scales = "free")
  
# Get the means from the max and the min

long_df_est %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density),
            max_density = max(density),
            ) %>% 
  ggplot(aes(x = PP, 
             y = mean_density))+
  geom_point()+
  facet_wrap(~ species, 
             scales = "free")


# Mean values of the entire cycle, excepting the ones on the first few
# time steps 
limit_cycles %>% 
  filter(species != "Ca" & species != "Cj",
         time <= 200) %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density)) %>% 
  ggplot(aes(x = PP, 
             y = mean_density))+
  geom_point()+
  facet_wrap(~ species, 
             scales = "free")


# Plot moose and caribou cycles with predator for extreme PP values
limit_cycles %>% 
  filter(species != "Ca" & species != "Cj",
         PP == 0 | PP == 1,
         time >= 100) %>% 
  ggplot(aes(x = P, 
             y = density,
             color = (time)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")


limit_cycles %>% 
  filter(time >= 100) %>% 
  group_by(PP, species) %>% 
  pivot_longer(P) %>% 
  summarize(mean_density = mean(density)) %>% 
  pivot_wider(names_from = species,
              values_from = mean_density) %>%
  ggplot(aes(x = PP,
             y = mean_density))+
  geom_point()+
  facet_wrap(~ species,
            scales = "free")


long_df_est %>% 
  filter(time >= 100) %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density)) %>% 
  pivot_wider(names_from = species,
              values_from = mean_density) %>%
  ggplot(aes(x = PP,
             y = mean_density))+
  geom_point()+
  facet_wrap(~ species,
             scales = "free")



wo_first_osc <- long_df_est %>% 
  filter(time >= 100) %>% 
  group_by(PP, species) %>% 
  summarize(mean_density = mean(density)) %>% 
  pivot_wider(names_from = species,
              values_from = mean_density) %>% 
  mutate(type = "wo_first_osc")

eq_densities <- long_df_est %>% 
  filter(time == 2000) %>% 
  group_by(PP) %>% 
  pivot_wider(names_from = species,
              values_from = density) %>% 
  mutate(type = "eq_dens")


test <- bind_rows(wo_first_osc,
                  eq_densities)

test %>% 
  pivot_longer(cols = c(4:10)) %>% 
  ggplot(aes(x = PP,
             y = value,
             color = type))+
  geom_point()+
  facet_wrap(~ name,
             scales = "free")+
  labs(y = "density")



# Limit cycles?
# Plot prey density against predator's

long_df_west <- long_df_west %>% 
  mutate(type = ifelse(species == "P", "predator", ifelse(species == "U" | species == "V", "vegetation", "prey")))


limit_cycles_west <- test %>% 
  pivot_longer(cols = c(5:8, 10, 11),
               names_to = "species",
               values_to = "density")


limit_cycles_west %>% 
  filter(species != "Ca" & species != "Cj") %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(PP)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")



# Test with longer simulations periods

# === West scenario ===============


setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")

# Load the df containing all the simulations 
all_simulations_west <- readRDS ("all_simulations_long_period.R")

# Unnest the result columns
unnested_west <- all_simulations_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)


# Without deers
test_west <- unnested_west %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)

# Melt the dataframe to a long format
long_df_west <- test_west %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")


# Create a facet plot of density of all species during time
# for PP from 0 to 10
ggplot(long_df_west, aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_grid(species ~ PP, scales = "free")



final_densities_east <- long_df %>% 
  filter(time == 800) %>% 
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap( ~ species, scales = "free")

final_densities_east

final_densities_east %>% 
  pivot_wider(names_from = species,
              values_from = density)


# Limit cycles?
# Plot prey density against predator's

long_df_est <- long_df_est %>% 
  mutate(type = ifelse(species == "P", "predator", ifelse(species == "U" | species == "V", "vegetation", "prey")))

limit_cycles <- test_est_long_period %>% 
  pivot_longer(cols = c(5:8, 10, 11),
               names_to = "species",
               values_to = "density")


limit_cycles %>% 
  filter(species != "Ca" & species != "Cj") %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(PP)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")


# Zoom on one of these cycle for comparison
limit_cycles <- test_est_long_period %>% 
  pivot_longer(cols = c(5:8, 10, 11),
               names_to = "species",
               values_to = "density")


# Plot moose and caribou cycles with predator for extreme PP values
limit_cycles %>% 
  filter(species =="Ma"| species == "Na",
         PP == 0 | PP == 1,
         P <= 0.03) %>% 
  ggplot(aes(x = P, 
             y = density,
             color = factor(species)))+
  geom_point()+
  facet_grid(  species ~ PP , scales = "free")+
  labs(x = "Predator density", 
       y = "Prey density",
       title = "Predator and prey cycle dynamics for different PP",
       subtitle = "title of the sub-plots are the prey species names")



# ==============================================================================
# Comparison with and wo deer

# Without deers
test_est <- subdata_unnested_wo_deer %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10) %>% 
  mutate(zone = "est")

# Melt the dataframe to a long format
long_df_est <- test_est %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")


# Without deers
test_west <- subdata_unnested %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10) %>% 
  mutate(zone = "west")

# Melt the dataframe to a long format
long_df_west <- test_west %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")



merged_est_west <- bind_rows(long_df_est,
                             long_df_west)

merged_est_west %>% 
  filter(time == 0 | time == 800) %>% 
  ggplot(aes(x = time))+
    geom_line(aes(y = density, color = zone))+
  facet_grid(species~PP, scales = "free")


# ==============================================================================
# Growth rate for our different PP Values

caribou_growth_rate_east <- subdata_unnested %>% 
  group_by(PP) %>% 
  filter(time == 0 | time == 800) %>% 
  mutate(growth_rate_N = Na - lag(Na),
         zone = "est")

caribou_growth_rate_west <- subdata_unnested_wo_deer %>% 
  group_by(PP) %>% 
  filter(time == 0 | time == 800) %>% 
  mutate(growth_rate_N = Na - lag(Na),
         zone = "west")


merged_growth_rate <-  bind_rows(caribou_growth_rate_east, 
                                 caribou_growth_rate_west)


merged_growth_rate <- merged_growth_rate %>% 
  mutate(diff_time = time - lag(time),
         Rate_percent = (growth_rate_N / diff_time)/Na * 100)


merged_growth_rate %>% 
  ungroup() %>% 
  ggplot(aes(x = PP,
             y = Rate_percent,
             color = zone))+
  geom_point()
