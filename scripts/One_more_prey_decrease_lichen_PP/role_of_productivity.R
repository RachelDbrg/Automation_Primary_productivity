library(readr)
library(tidyverse)
library(viridis)
library(ggh4x)

theme_set(theme_minimal())

# ==============================================================================
# Load all the datafiles for the western simulations

# Directory where your RDS files are located
directory <- "~/Automation_Primary_Productivity/Messier_figure/"

# List RDS files in the directory that starts with "simA"
west_files <- list.files(path = directory, pattern = "^simB", full.names = TRUE)

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


# List RDS files in the directory that starts with "simA"
west_data_frame_names <- ls(pattern = "^simB")

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
    mutate_all(as.numeric) %>% 
    # mutate(sc = str_sub(data_frame_name, end = -14))
    mutate(sc = str_sub(data_frame_name),
           zone = "west")
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}


simB1 <- modified_data_frames[["simB1"]]
simB1 <- simB1 %>% 
  mutate(delta = 0.01)

simB2 <- modified_data_frames[["simB2"]]
simB2 <- simB2 %>% 
  mutate(delta = 0.05)

simB3 <- modified_data_frames[["simB3"]]
simB3 <- simB3 %>% 
  mutate(delta = 0.1)

simB4 <- modified_data_frames[["simB4"]]
simB4 <- simB4 %>% 
  mutate(delta = 0.5)

simB5 <- modified_data_frames[["simB5"]]
simB5 <- simB5 %>% 
  mutate(delta = 1)

simB6 <- modified_data_frames[["simB6"]]
simB6 <- simB6 %>% 
  mutate(delta = 5)

simB7 <- modified_data_frames[["simB7"]]
simB7 <- simB7 %>% 
  mutate(delta = 10)

simB8 <- modified_data_frames[["simB8"]]
simB8 <- simB8 %>% 
  mutate(delta = 50)

simB9 <- modified_data_frames[["simB9"]]
simB9 <- simB9 %>% 
  mutate(delta = 2)

simB10 <- modified_data_frames[["simB10"]]
simB10 <- simB10 %>% 
  mutate(delta = 3)

simB11 <- modified_data_frames[["simB11"]]
simB11 <- simB11 %>% 
  mutate(delta = 4)

simB12 <- modified_data_frames[["simB12"]]
simB12 <- simB12 %>% 
  mutate(delta = 6)

simB13 <- modified_data_frames[["simB13"]]
simB13 <- simB13 %>% 
  mutate(delta = 7)

simB14 <- modified_data_frames[["simB14"]]
simB14 <- simB14 %>% 
  mutate(delta = 8)

simB15 <- modified_data_frames[["simB15"]]
simB15 <- simB15 %>% 
  mutate(delta = 9)

simB16 <- modified_data_frames[["simB16"]]
simB16 <- simB16 %>% 
  mutate(delta = 6.2)

simB17 <- modified_data_frames[["simB17"]]
simB17 <- simB17 %>% 
  mutate(delta = 6.4)

simB18 <- modified_data_frames[["simB18"]]
simB18 <- simB18 %>% 
  mutate(delta = 6.6)

simB19 <- modified_data_frames[["simB19"]]
simB19 <- simB19 %>% 
  mutate(delta = 6.8)



simB5 %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time,
             y = density))+
  geom_line()+
  facet_grid(species~PP, 
             scales="free")


test_west <- do.call("rbind", mget(ls(pattern = "^simB")))



test_west %>% 
  mutate(across(!sc, as.numeric)) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>%
  filter(time == 2000,
         species %in% c("Ma")) %>% 
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_wrap(~sc, scales = "free")



# ==============================================================================

# Load all the datafiles for the eastern simulations

# Directory where your RDS files are located
directory <- "~/Automation_Primary_Productivity/Messier_figure/"

# List RDS files in the directory that starts with "simA"
east_files <- list.files(path = directory, pattern = "^simA", full.names = TRUE)

# Create a list to store the data
east_data_list <- list()

# Loop through each file and read it into R
for (file_path in east_files) {
  # Extract the name without the directory and extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the RDS file and store it in the list
  east_data_list[[file_name]] <- readRDS(file_path)
  
  list2env(east_data_list, envir = .GlobalEnv)
}


# List RDS files in the directory that starts with "simA"
east_data_frame_names <- ls(pattern = "^simA")

# Create a list to store the modified data frames
modified_data_frames <- list()

# Loop through each data frame and apply the operations
for (data_frame_name in east_data_frame_names) {
  # Get the data frame by its name
  data_frame <- get(data_frame_name)
  # print(data_frame_name)
  
  # Apply the operations
  modified_data_frame <- data_frame %>%
    pull(outputs) %>%
    map_dfr(as.data.frame) %>% 
    mutate_all(as.numeric) %>% 
    # mutate(sc = str_sub(data_frame_name, end = -14))
    mutate(sc = str_sub(data_frame_name),
           zone = "est")
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}


simA1 <- modified_data_frames[["simA1"]]
simA1 <- simA1 %>% 
  mutate(delta = 0.01)

simA2 <- modified_data_frames[["simA2"]]
simA2 <- simA2 %>% 
  mutate(delta = 0.05)

simA3 <- modified_data_frames[["simA3"]]
simA3 <- simA3 %>% 
  mutate(delta = 0.1)

simA4 <- modified_data_frames[["simA4"]]
simA4 <- simA4 %>% 
  mutate(delta = 0.5)

simA5 <- modified_data_frames[["simA5"]]
simA5 <- simA5 %>% 
  mutate(delta = 1)

simA6 <- modified_data_frames[["simA6"]]
simA6 <- simA6 %>% 
  mutate(delta = 5)

simA7 <- modified_data_frames[["simA7"]]
simA7 <- simA7 %>% 
  mutate(delta = 10)

simA8 <- modified_data_frames[["simA8"]]
simA8 <- simA8 %>% 
  mutate(delta = 50)

simA9 <- modified_data_frames[["simA9"]]
simA9 <- simA9 %>% 
  mutate(delta = 2)

simA10 <- modified_data_frames[["simA10"]]
simA10 <- simA10 %>% 
  mutate(delta = 3)

simA11 <- modified_data_frames[["simA11"]]
simA11 <- simA11 %>% 
  mutate(delta = 4)

simA12 <- modified_data_frames[["simA12"]]
simA12 <- simA12 %>% 
  mutate(delta = 6)

simA13 <- modified_data_frames[["simA13"]]
simA13 <- simA13 %>% 
  mutate(delta = 7)

simA14 <- modified_data_frames[["simA14"]]
simA14 <- simA14 %>% 
  mutate(delta = 8)

simA15 <- modified_data_frames[["simA15"]]
simA15 <- simA15 %>% 
  mutate(delta = 9)




simA5 %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time,
             y = density))+
  geom_line()+
  facet_grid(species~PP, 
             scales="free")



test_est <- do.call("rbind", mget(ls(pattern = "^simA")))


# General overview for all species
test_est %>% 
  mutate(across(!sc, as.numeric)) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>%
  filter(time == 2000,
         !species %in% c("Ca", "Cj")) %>% 
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_wrap(~species, scales = "free")


# Look at species in particular
test_est %>%
  mutate(across(!sc, as.numeric)) %>%
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>%
  filter(time == 2000,
         species %in% c("Ma")) %>%
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_wrap(~sc, scales = "free")



# ==============================================================================
# same_sc_comp <- bind_rows(simB5, simA5)
same_sc_comp <- bind_rows(simB3, simA3)

same_sc_comp %>% 
  filter(time == 2000)


same_sc_comp %>% 
  pivot_longer(cols = c(P,27:29),
               names_to = "species",
               values_to = "density") %>% 
  filter(!species %in% c("Ca", "Cj"),
         time >= 50,
         time <= 200) %>% 
  ggplot(aes(x = time,
             y = density,
             color = sc))+
  geom_line()+
  facet_grid(species~PP, 
             scales="free")



# Pour une espece, regarder l'evolution en fonction des differents scenarios

test_est <- test_est %>% 
  mutate(zone="est")

test_west <- test_west %>% 
  mutate(zone="west")

all <- bind_rows(test_est, test_west)


all %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(time <= 300,
         PP %in% c("0", "1"),
         delta == 1) %>% 
  ggplot(aes(x = time,
             y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, 
             scales="free")


all %>% 
  filter(PP == 1,
         delta ==1) %>% 
  select(Ma, time, zone) %>% 
  mutate(across(!zone, as.numeric)) %>% 
  arrange(asc("Ma")) %>% 
  head(10)



# Compute the diff between final densities for each species, for each PP value

df <- same_sc_comp %>% 
  select(c(1:10, PP, sc)) %>% 
  filter(time == 2000) %>%
  group_by(sc) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") 



density_difference <- df %>%
  group_by(species,PP) %>%
  summarize(density_diff = abs(diff(density)))



# ==============================================================================
# ==============================================================================
same_sc_comp <- bind_rows(simB5, simA5,
                          simB6, simA6,
                          simB3, simA3)



diff_scenarios <- same_sc_comp %>% 
  select(PP, delta) %>% 
  mutate(test = paste("PP=", as.character(PP), " ; delta=", as.character(delta), sep = ""))



# diff_scenarios$test <- factor(diff_scenarios$test, levels = c("PP=0; delta=0.1", "PP=1; delta=0.1", "PP=0; delta=1", "PP=1; delta=1", "PP=0; delta=5", "PP=1; delta=5"))

same_sc_comp <- same_sc_comp %>% 
  mutate(tidy_sc = diff_scenarios$test) 


graph_comparaison <- same_sc_comp %>% 
  mutate(tidy_sc = diff_scenarios$test,
         Zone = recode(zone, est = 'East', west = 'West'),
         C_tot = Cj + Ca,
         N_tot = Nj + Na,
         M_tot = Mj + Ma) %>% 
  rename(Deer = C_tot,
         Caribou = N_tot,
         Moose = M_tot,
         Wolf = P) %>% 
  pivot_longer(cols = c(Wolf, 27:29),
               names_to = "species",
               values_to = "density") %>% 
  filter(time >= 50,
         time <= 300,
         PP %in% c("0", "1")) %>% 
  ggplot(aes(x = time,
             y = density,
             color = Zone))+
  geom_line()+
  geom_vline(xintercept = 100, colour = "black", linetype = 2, linewidth = 1) +
  facet_grid2(species~tidy_sc, 
             axes="all",
             scales = "free",
             independent = "y", space = "fixed")+
  labs(title = "Species densities for different values of productivity and accessibility to the resource",
       subtitle = "The dash line represent the time when perturbation occurs",
       color= "Zone",
       x = "Time (years)",
       y = "Species density (ind/km2)")
  
graph_comparaison

# RONMMER AXES  + TITRES FIGURE



# ==============================================================================

same_sc_comp %>% 
  filter(sc == "simA3" | sc == "simB3",
         PP == 0) %>% 
  mutate(tidy_sc = sc,
         Zone = recode(zone, est = 'East', west = 'West'),
         C_tot = Cj + Ca,
         N_tot = Nj + Na,
         M_tot = Mj + Ma) %>% 
  rename(Deer = C_tot,
         Caribou = N_tot,
         Moose = M_tot,
         Wolf = P) %>% 
  pivot_longer(cols = c(Wolf, 27:29),
               names_to = "species",
               values_to = "density") %>%  
  ggplot(aes(x = time,
             y = density,
             color = sc))+
  geom_line()+
  geom_vline(xintercept = 100, colour = "black", linetype = 2, linewidth = 1) +
  facet_grid2(species~tidy_sc, 
              axes="all",
              scales = "free",
              independent = "y", space = "fixed")+
  labs(title = "Species densities for different values of productivity and accessibility to the resource",
       subtitle = "The dash line represent the time when perturbation occurs",
       color= "Zone",
       x = "Time (years)",
       y = "Species density (ind/km2)")


# ==============================================================================
# Select only the caribou and wolf

# levels(subset$tidy_sc)

# subset$tidy_sc <- as.factor(subset$tidy_sc)


# test_lvl <- factor(c("PP=0; delta=0.1", "PP=1; delta=0.1", "PP=0; delta=1", "PP=1; delta=1", "PP=0; delta=5", "PP=1; delta=5"))


# subsets$test_lvl <- factor(subset$tidy_sc, levels = c("PP=0; delta=0.1", "PP=1; delta=0.1", "PP=0; delta=1", "PP=1; delta=1", "PP=0; delta=5", "PP=1; delta=5"))


subset <- same_sc_comp %>% 
  mutate(tidy_sc = diff_scenarios$test,
         Zone = recode(zone, est = 'East', west = 'West'),
         C_tot = Cj + Ca,
         N_tot = Nj + Na,
         M_tot = Mj + Ma) %>% 
  rename(Deer = C_tot,
         Caribou = N_tot,
         Moose = M_tot,
         Wolf = P) %>% 
  pivot_longer(cols = c(Wolf, 27:29),
               names_to = "species",
               values_to = "density") %>% 
  filter(time >= 50,
         time <= 300,
         PP %in% c("0", "1"),
         species %in% c("Caribou", "Wolf"))


subset %>% 
  ggplot(aes(x = time,
           y = density,
           color = Zone))+
  geom_line()+
  geom_vline(xintercept = 100, colour = "black", linetype = 2, linewidth = 1) +
  facet_grid(species~size_f,
              scales = "free")+
  labs(title = "Species densities for different values of productivity and accessibility to the resource",
       subtitle = "The dash line represent the time when perturbation occurs",
       color= "Zone",
       x = "Time (years)",
       y = "Species density (ind/km2)")

graph_comparaison



graph_comparaison <- same_sc_comp %>% 
  mutate(tidy_sc = diff_scenarios$test,
         Zone = recode(zone, est = 'East', west = 'West'),
         C_tot = Cj + Ca,
         N_tot = Nj + Na,
         M_tot = Mj + Ma) %>% 
  rename(Deer = C_tot,
         Caribou = N_tot,
         Moose = M_tot,
         Wolf = P) %>% 
  pivot_longer(cols = c(Wolf, 27:29),
               names_to = "species",
               values_to = "density") %>% 
  filter(time >= 50,
         time <= 300,
         PP %in% c("0", "1"),
         species %in% c("Caribou", "Wolf")) %>% 
  ggplot(aes(x = time,
             y = density,
             color = Zone))+
  geom_line()+
  geom_vline(xintercept = 100, colour = "black", linetype = 2, linewidth = 1) +
  facet_grid2(species~tidy_sc, 
              axes="all",
              scales = "free",
              independent = "y", space = "fixed")+
  labs(title = "Species densities for different values of productivity and accessibility to the resource",
       subtitle = "The dash line represent the time when perturbation occurs",
       color= "Zone",
       x = "Time (years)",
       y = "Species density (ind/km2)")

graph_comparaison



graph_comparaison

graph_comparaison <- same_sc_comp %>% 
  mutate(tidy_sc = diff_scenarios$test) %>% 
  pivot_longer(cols = c(P, 27:29),
               names_to = "species",
               values_to = "density") %>% 
  filter(time >= 50,
         time <= 300,
         PP %in% c("0", "1")) %>% 
  ggplot(aes(x = time,
             y = density,
             color = zone))+
  geom_line()+
  facet_grid(PP~species, scales = "free")

graph_comparaison


# ==============================================================================
# Percentage of diminution in population density with a perturbation

max_values <- same_sc_comp %>% 
  filter(PP %in% c("0","1",
         delta ==1)) %>% 
  group_by(zone,PP) %>% 
  filter(time == 100) %>% 
  select(c(27:29))


min_values <- same_sc_comp %>% 
  filter(PP %in% c("1",
                   delta ==1)) %>% 
  group_by(zone,PP,time) %>% 
  filter(time > 100) %>% 
  select(c(27:29))


# Check for PP = 0, delta = 1 
rank_min_caribou <- which.min(min_values$N_tot)

min_values[rank_min_caribou,]
# Time where the minimum species density is reached is 155

min_values <- same_sc_comp %>% 
  filter(PP %in% c("1",
                   delta ==1)) %>% 
  group_by(zone,PP,time) %>% 
  filter(time == 114) %>% 
  select(c(27:29))



simA1 %>% 
  group_by(PP, sc) %>% 
  ggplot(aes(x = time,
             y = M_tot))+
  geom_line()+
  facet_wrap(~sc)



# Look at species in particular
test_est %>%
  mutate(across(!sc, as.numeric)) %>%
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>%
  filter(time == 2000,
         species %in% c("Ma")) %>%
  ggplot(aes(x = PP,
             y = density,
             color = sc))+
  geom_point()+
  facet_wrap(~sc, scales = "free")


simA1 %>% 
  filter (time == 2000,
          PP == 0) %>% View()



simA1 %>% 
  group_by(PP, sc) %>% 
  ggplot(aes(x = time,
             y = Ma))+
  geom_line()+
  facet_wrap(~sc)




same_sc_comp$sc

# =================

same_sc_comp <- bind_rows(simB5, simA5,
                          simB6, simA6,
                          simB3, simA3)
  


test_graph <- same_sc_comp %>% 
  mutate(tidy_sc = diff_scenarios$test,
       Zone = recode(zone, est = 'East', west = 'West'),
       C_tot = Cj + Ca,
       N_tot = Nj + Na,
       M_tot = Mj + Ma) %>% 
  rename(Deer = C_tot,
         Caribou = N_tot,
         Moose = M_tot,
         Wolf = P) %>% 
  pivot_longer(cols = c(Wolf, 27:29),
               names_to = "species",
               values_to = "density") %>% 
  filter(time <= 800,
         PP %in% c("0", "1"),
         sc == "simB5" | sc == "simA5")



test_graph %>% 
  ggplot(aes(x = time,
             y = density,
             color = Zone))+
  geom_line()+
  # geom_vline(xintercept = 100, colour = "black", linetype = 2, linewidth = 1) +
  facet_grid(species~PP, 
             scales="free")+
  labs(title = "Species densities for different values of productivity and accessibility to the resource",
       subtitle = "The dash line represent the time when perturbation occurs",
       color= "Zone",
       x = "Time (years)",
       y = "Species density (ind/km2)")

# ====================


