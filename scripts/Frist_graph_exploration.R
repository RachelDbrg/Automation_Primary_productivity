rm(list = ls(all.names = TRUE))

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")
setwd("~/Automation_Primary_productivity/res_simulations")

library(tidyverse)

# Load the df containing all the simulations 
all_simulations <- readRDS ("all_simulations.R")
all_simulations_wo_deer <- readRDS ("all_simulations_without_deer.R")
all_simulations_sc_1 <- readRDS ("all_simulations_scenario1.R")

# Subset the data
# PP values that we want to look at

subdata <- all_simulations %>% 
  filter(PP %in% c(seq(0,10,1)))

subdata_wo_deer <- all_simulations_wo_deer %>% 
  filter(PP %in% c(seq(0,10,1)))

# For each of the PP scenarios, extract the final densities (ie the densities
# obtained at the last time step)

# ------------------------------------------------------------------------------
# --------------------------- Time series --------------------------------------
# ------------------------------------------------------------------------------

# Unnest the result columns
subdata_unnested <- subdata %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)

subdata_unnested_wo_deer <- subdata_wo_deer %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)


sub <- all_simulations_sc_1 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)

# Select only columns with species density
test <- subdata_unnested %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)

# Without deers
test <- subdata_unnested_wo_deer %>% 
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)


# Melt the dataframe to a long format
long_df <- test %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")


# Create a facet plot of density of all species during time
# for PP from 0 to 10
ggplot(long_df, aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Species Density Over Time", x = "Time", y = "Density") +
  facet_wrap(~species, scales = "free")


# ------------------------------------------------------------------------------
# For each species, I want to plot the graph of the evolution of its 
# density for different PP values

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/lichen.png"
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/lichen_wo_deer.png"


# Lichen
# lichen_time_serie <- subdata_unnested %>% 
lichen_time_serie <- subdata_unnested_wo_deer %>%
  dplyr::select(c(PP,time, V)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(V),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Lichen Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_wrap(~species)+
  theme_minimal()

# Display plot
lichen_time_serie

# Save the plot as a PNG file
ggsave(filename = save_path, plot = lichen_time_serie, width = 8, height = 6)


# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/deciduous.png"
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/deciduous_wo_deer.png"

# Deciduous
# deciduous_time_serie <- subdata_unnested %>%
deciduous_time_serie <- subdata_unnested_wo_deer %>% 
  dplyr::select(c(PP,time, U)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(U),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Deciduous Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_wrap(~species)+
  theme_minimal()

# Display plot
deciduous_time_serie

# Save the plot as a PNG file
ggsave(filename = save_path, plot = deciduous_time_serie, width = 8, height = 6)


# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/moose.png"
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/moose_wo_deer.png"

# Moose
# moose_time_serie <- subdata_unnested %>% 
moose_time_serie <- subdata_unnested_wo_deer %>% 
  dplyr::select(c(PP,time, Ma, Mj)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(Ma,Mj),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Moose Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_wrap(~species)+
  theme_minimal()

# Display plot
moose_time_serie

# Save the plot as a PNG file
ggsave(filename = save_path, plot = moose_time_serie, width = 8, height = 6)

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/caribou.png"
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/caribou_wo_deer.png"

# Caribou
# caribou_time_serie <- subdata_unnested %>%
caribou_time_serie <- subdata_unnested_wo_deer %>% 
  dplyr::select(c(PP,time, Na, Nj)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(Na,Nj),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Caribou Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_wrap(~species)+
  theme_minimal()

# Display plot
caribou_time_serie

# Save the plot as a PNG file
ggsave(filename = save_path, plot = caribou_time_serie, width = 8, height = 6)

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/deer.png"

# Deer
deer_time_serie <- subdata_unnested %>% 
  dplyr::select(c(PP,time, Ca, Cj)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(Ca,Cj),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Deer Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_wrap(~species, scales = "free")+
  theme_minimal()

# Display plot
deer_time_serie

# Save the plot as a PNG file
ggsave(filename = save_path, plot = deer_time_serie, width = 8, height = 6)


# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/wolf.png"
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/wolf_wo_deer.png"

# Wolf
# wolf_time_serie <- subdata_unnested %>% 
wolf_time_serie <- subdata_unnested_wo_deer %>% 
  dplyr::select(c(PP,time,P)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(P),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(PP))) +
  geom_line() +
  labs(title = "Wolf Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_wrap(~species, scales = "free")+
  theme_minimal()

# Display plot
wolf_time_serie

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_time_serie, width = 8, height = 6)


# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Time_series/PP_0_10_wo_deer.png"

# For a same PP value, see the difference between PP = 0 and PP = 10
PP_0_10 <- subdata_unnested_wo_deer %>% 
  dplyr::select(c(PP,time:Cj)) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(V:Cj),
               names_to = "species",
               values_to = "density") %>% 
  filter(PP %in% c(0,10)) %>% 
  ggplot(., aes(x = time, y = density, color = as.factor(species))) +
  geom_line() +
  labs(title = "Wolf Density Over Time, for several PP", x = "Time", y = "Density") +
  scale_color_discrete(name = "PP") +
  facet_grid(vars(species), vars(PP),  scales = "free")+
  theme_minimal()

PP_0_10

# Save the plot as a PNG file
ggsave(filename = save_path, plot = PP_0_10, width = 8, height = 6)


# ------------------------------------------------------------------------------
# --------------------------- Phase space --------------------------------------
# ------------------------------------------------------------------------------

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_moose_wolf_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_moose_wolf_PP_0_10_wo_deer.png"

# Adult_moose_wolf
# phase_plot_adult_moose_wolf <- subdata_unnested %>% 
phase_plot_adult_moose_wolf <- subdata_unnested_wo_deer %>% 
  mutate_all(as.numeric) %>% 
  dplyr::select(c(PP, time, Ma, P)) %>% 
  ggplot(aes(x=Ma, y=P, color =time)) +
  geom_point()+
  facet_wrap(~PP)+
  labs(title = "Phase plot adult moose vs. wolf", x = "Adult moose density", y = "Wolf density") +
  theme_minimal()


phase_plot_adult_moose_wolf

# Save the plot as a PNG file
ggsave(filename = save_path, plot = PP_0_10, width = 8, height = 6)


# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_moose_wolf_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_moose_wolf_PP_0_10_wo_deer.png"

# Juvenile_moose_wolf
# phase_plot_juvenile_moose_wolf <- subdata_unnested %>%
  phase_plot_juvenile_moose_wolf <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Mj, P)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot juvenile moose vs. wolf", x = "Juvenile moose density", y = "Wolf density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_juvenile_moose_wolf

# Save the plot as a PNG file
ggsave(filename = save_path, plot = PP_0_10, width = 8, height = 6)


# Juvenile_caribou_wolf

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_caribou_wolf_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_caribou_wolf_PP_0_10_wo_deer.png"

# phase_plot_juvenile_caribou_wolf <- subdata_unnested %>%
phase_plot_juvenile_caribou_wolf <- subdata_unnested_wo_deer%>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Nj, P)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot juvenile caribou vs. wolf", x = "Juvenile caribou density", y = "Wolf density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_juvenile_caribou_wolf


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_juvenile_caribou_wolf, width = 8, height = 6)

# Adult_caribou_wolf

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_caribou_wolf_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_caribou_wolf_PP_0_10_wo_deer.png"

# phase_plot_adult_caribou_wolf <- subdata_unnested %>%
phase_plot_adult_caribou_wolf <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Na, P)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot adult caribou vs. wolf", x = "Adult caribou density", y = "Wolf density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_adult_caribou_wolf


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_adult_caribou_wolf, width = 8, height = 6)

# Adult_deer_wolf

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_deer_wolf_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_deer_wolf_PP_0_10_wo_deer.png"

# phase_plot_adult_deer_wolf <- subdata_unnested %>%
phase_plot_adult_deer_wolf <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Ca, P)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot adult deer vs. wolf", x = "Adult deer density", y = "Wolf density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_adult_deer_wolf


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_adult_deer_wolf, width = 8, height = 6)



# Juvenile_deer_wolf

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_deer_wolf_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_deer_wolf_PP_0_10_wo_deer.png"

# phase_plot_juvenile_deer_wolf <- subdata_unnested %>%
phase_plot_juvenile_deer_wolf <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Cj, P)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot juvenile deer vs. wolf", x = "Juvenile deer density", y = "Wolf density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_juvenile_deer_wolf


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_juvenile_deer_wolf, width = 8, height = 6)


# Deciduous_moose

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/deciduous_moose_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/deciduous_moose_PP_0_10_wo_deer.png"

# phase_plot_deciduous_moose <- subdata_unnested %>%
phase_plot_deciduous_moose <- subdata_unnested_deer %>%
  mutate_all(as.numeric) %>%
  mutate(M_tot = Mj+Ma) %>% 
  dplyr::select(c(PP, time, U, M_tot)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot deciduous vs. moose", x = "Deciduous density", y = "Moose density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_deciduous_moose


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_deciduous_moose, width = 8, height = 6)


# Deciduous_deer

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/deciduous_deer_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/deciduous_deer_PP_0_10_wo_deer.png"

# phase_plot_deciduous_deer <- subdata_unnested %>%
phase_plot_deciduous_deer <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  mutate(C_tot = Cj+Ca) %>% 
  dplyr::select(c(PP, time, U, C_tot)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot deciduous vs. deer", x = "Deciduous density", y = "Deer density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_deciduous_deer


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_deciduous_deer, width = 8, height = 6)


# Lichen_caribou

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/lichen_caribou_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/lichen_caribou_PP_0_10_wo_deer.png"

# phase_plot_lichen_caribou <- subdata_unnested %>%
phase_plot_lichen_caribou <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  mutate(N_tot = Nj+Na) %>% 
  dplyr::select(c(PP, time, U, N_tot)) %>%
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Mj, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot lichen vs. caribou", x = "Lichen density", y = "Caribou density") +
  scale_color_continuous(name = "time")+
  theme_minimal()

phase_plot_lichen_caribou


# Save the plot as a PNG file
ggsave(filename = save_path, plot = phase_plot_lichen_caribou, width = 8, height = 6)


# Wolf functional response
# On adult moose

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_moose_wolf_rfonc_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_moose_wolf_rfonc_PP_0_10_wo_deer.png"

# wolf_functional_response_adult_moose <- subdata_unnested %>%
wolf_functional_response_adult_moose <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Ma, rfonc_P_Ma)) %>% 
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Ma, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot adult moose density vs. wolf functional response", x = "Moose density", y = "Wolf functional response") +
  scale_color_continuous(name = "time")+
  theme_minimal()

wolf_functional_response_adult_moose

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_functional_response_adult_moose, width = 8, height = 6)


# On juvenile moose

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_moose_wolf_rfonc_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_moose_wolf_rfonc_PP_0_10_wo_deer.png"

# wolf_functional_response_juvenile_moose <- subdata_unnested %>%
wolf_functional_response_juvenile_moose <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Mj, rfonc_P_Mj)) %>% 
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Ma, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot juvenile moose density vs. wolf functional response", x = "Moose density", y = "Wolf functional response") +
  scale_color_continuous(name = "time")+
  theme_minimal()

wolf_functional_response_juvenile_moose

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_functional_response_juvenile_moose, width = 8, height = 6)
  
  
# On juvenile caribou

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_caribou_wolf_rfonc_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_caribou_wolf_rfonc_PP_0_10_wo_deer.png"

# wolf_functional_response_juvenile_caribou <- subdata_unnested %>%
wolf_functional_response_juvenile_caribou <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Nj, rfonc_P_Nj)) %>% 
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Ma, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot juvenile caribou density vs. wolf functional response", x = "Caribou density", y = "Wolf functional response") +
  scale_color_continuous(name = "time")+
  theme_minimal()

wolf_functional_response_juvenile_caribou

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_functional_response_juvenile_caribou, width = 8, height = 6)

  
# On adult caribou

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_caribou_wolf_rfonc_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_caribou_wolf_rfonc_PP_0_10_wo_deer.png"

# wolf_functional_response_adult_caribou <- subdata_unnested %>%
wolf_functional_response_adult_caribou <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Na, rfonc_P_Na)) %>% 
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Ma, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot adult caribou density vs. wolf functional response", x = "Caribou density", y = "Wolf functional response") +
  scale_color_continuous(name = "time")+
  theme_minimal()

wolf_functional_response_adult_caribou

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_functional_response_adult_caribou, width = 8, height = 6)
  
  
# On adult deer

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_deer_wolf_rfonc_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/adult_deer_wolf_rfonc_PP_0_10_wo_deer.png"

# wolf_functional_response_adult_deer <- subdata_unnested %>%
wolf_functional_response_adult_deer <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Ca, rfonc_P_Ca)) %>% 
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Ma, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot adult deer density vs. wolf functional response", x = "Deer density", y = "Wolf functional response") +
  scale_color_continuous(name = "time")+
  theme_minimal()

wolf_functional_response_adult_deer

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_functional_response_adult_deer, width = 8, height = 6)

  

# On juvenile deer

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_deer_wolf_rfonc_PP_0_10.png"
save_path <- "~/Automation_Primary_productivity/Plots/Phase_space/juvenile_deer_wolf_rfonc_PP_0_10_wo_deer.png"

# wolf_functional_response_juvenile_deer <- subdata_unnested %>%
wolf_functional_response_juvenile_deer <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>%
  dplyr::select(c(PP, time, Cj, rfonc_P_Cj)) %>% 
  ggplot(aes(x = .[[3]], y = .[[4]], color = .[[2]])) + # x = pick the 3rd column: Ma, etc. In the same order of selection by dplyr::select, just above
  geom_point() +
  facet_wrap(~PP) +
  labs(title = "Phase plot juvenile deer density vs. wolf functional response", x = "Deer density", y = "Wolf functional response") +
  scale_color_continuous(name = "time")+
  theme_minimal()

wolf_functional_response_juvenile_deer

# Save the plot as a PNG file
ggsave(filename = save_path, plot = wolf_functional_response_juvenile_deer, width = 8, height = 6)



# ------------------------------------------------------------------------------
# -------------------- Look at the final densities -----------------------------
# ------------------------------------------------------------------------------

# Pick the final densities 

# Time series
subdata_unnested <- all_simulations %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)


# test <- subdata_unnested %>%
test <- subdata_unnested_wo_deer %>%
  mutate_all(as.numeric) %>% 
  group_by(PP) %>% 
  dplyr::select(1:10)


# Melt the dataframe to a long format
long_df <- test %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density")

long_df_final_times <- long_df %>% 
  filter(time == 800)

# Create a ggplot
p <- ggplot(long_df_final_times, aes(x = PP, y = density)) +
  geom_point()+
  geom_smooth(method= "lm") +
  labs(title = "Final Species density according to PP value", x = "PP", y = "Final density") +
  facet_wrap(~species, scales = "free")

p

# Define the file path and name for saving the plot
save_path <- "~/Automation_Primary_productivity/Plots/final_densities_all_species_wo_deer.png"
# save_path <- "~/Automation_Primary_productivity/Plots/final_densities_all_species.png"
# Save the plot as a PNG file
ggsave(filename = save_path, plot = p, width = 8, height = 6)


ggplot_build(p)$data

# Build a function to extract the coordinates of the linear models
fit_lm_and_extract_equation <- function(data) {
  lm_fit <- lm(density ~ PP, data = data)
  eq <- as.character(lm_fit$coefficients[1]) %>%
    paste0(" + ", as.character(lm_fit$coefficients[2]), " * x")
  return(eq)
}

# Apply the function to our data (shown in the facet plot)
equations <- p$data %>%
  split(.$species) %>%
  map(fit_lm_and_extract_equation)



# ------------------------------------------------------------------------------¸
# Safety in number -- apparent mutualism?

subdata_unnested <- all_simulations %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)

subdata_unnested_wo_deer <- all_simulations_wo_deer %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame)

apparent_mutualism <- subdata_unnested %>% 
  mutate(all_prey_wo_caribou = Ma + Mj + Ca + Cj, 
         rfonc_caribou = rfonc_P_Nj + rfonc_P_Na) %>% 
  dplyr::select(PP, time, all_prey_wo_caribou, rfonc_P_Cj, rfonc_P_Ca,
                rfonc_P_Mj, rfonc_P_Ma, rfonc_P_Nj, rfonc_P_Na, rfonc_caribou,
                Ma, Mj, Ca, Cj, Na, Nj) %>% 
  mutate_all(as.numeric) %>% 
  mutate(moose_density = Ma+Mj)


apparent_mutualism_wo_deer <- subdata_unnested_wo_deer %>% 
  mutate(all_prey_wo_caribou = Ma + Mj + Ca + Cj, 
         rfonc_caribou = rfonc_P_Nj + rfonc_P_Na) %>% 
  dplyr::select(PP, time, all_prey_wo_caribou, rfonc_P_Cj, rfonc_P_Ca,
                rfonc_P_Mj, rfonc_P_Ma, rfonc_P_Nj, rfonc_P_Na, rfonc_caribou,
                Ma, Mj, Ca, Cj, Na, Nj) %>% 
  mutate_all(as.numeric) %>% 
  mutate(moose_density = Ma+Mj)



apparent_mutualism_wo_deer %>% 
  ggplot(aes(x = all_prey_wo_caribou,
             y = rfonc_caribou,
             color = as.factor(PP)))+
  geom_point()


# What are the values for which the functional response is the highest?
high_rfonc <- apparent_mutualism %>% 
  group_by(PP) %>% 
  filter(rfonc_caribou == max(rfonc_caribou)) %>% 
  mutate(scenario = "deer")


high_rfonc_no_deer <- apparent_mutualism_wo_deer %>% 
  group_by(PP) %>% 
  filter(rfonc_caribou == max(rfonc_caribou)) %>% 
  mutate(scenario = "nodeer")


merge_high_rfonc <- full_join(high_rfonc,
                              high_rfonc_no_deer)


merge_high_rfonc %>% 
  ggplot(aes(x = PP,
             y = rfonc_caribou,
             color = as.factor(scenario)))+
  geom_point()


merge_high_rfonc %>% 
  ggplot(aes(x = all_prey_wo_caribou,
             y = rfonc_caribou,
             color = as.factor(PP)))+
  geom_point()+
  facet_grid(~scenario,
             scales = "free")



merge_high_rfonc <- merge_high_rfonc %>% 
  mutate(moose_density = Ma+Mj)


# 3d version of it

library(rgl)
library(plotly)

# Create example data
x <- merge_high_rfonc$all_prey_wo_caribou
y <- merge_high_rfonc$rfonc_caribou
z <- merge_high_rfonc$PP


plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=merge_high_rfonc$moose_density)


# What is the strongest correlation? Rfonc on caribou and moose density or total
# prey density?


merge_high_rfonc %>% 
  ggplot(aes(x = moose_density,
             y = rfonc_caribou,
             color = PP,
             shape = as.factor(scenario)))+
  geom_point()


# Correlation plot

install.packages("corrplot")
library(corrplot)

cor_matrix <- cor(apparent_mutualism)
corrplot(cor_matrix, method = "color", type = "full", order = "hclust")



tre <- apparent_mutualism_wo_deer %>% 
  dplyr::select(-c(Ca,Cj,
                   rfonc_P_Ca, rfonc_P_Cj))

cor_matrix <- cor(tre)
corrplot(cor_matrix, method = "color", type = "full", order = "hclust")



# Full correlation plot

corr <- subdata_unnested %>% 
  dplyr::select(1:31)

cor_matrix <- cor(corr)
corrplot(cor_matrix, method = "color", type = "full", order = "hclust")


corr <- subdata_unnested_wo_deer %>% 
  dplyr::select(c(1:8),c(11:14), c(17:21), c(24:28), c(30:31))

cor_matrix <- cor(corr)
corrplot(cor_matrix, method = "color", type = "full", order = "hclust")
