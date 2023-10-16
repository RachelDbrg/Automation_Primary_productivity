library(readr)
library(tidyverse)
library(viridis)


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
    mutate(sc = str_sub(data_frame_name))
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}


simB1 <- modified_data_frames[["simB1"]]
simB2 <- modified_data_frames[["simB2"]]
simB3 <- modified_data_frames[["simB3"]]
simB4 <- modified_data_frames[["simB4"]]
simB5 <- modified_data_frames[["simB5"]]
simB6 <- modified_data_frames[["simB6"]]
simB7 <- modified_data_frames[["simB7"]]
simB8 <- modified_data_frames[["simB8"]]



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
    mutate(sc = str_sub(data_frame_name))
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}


simA1 <- modified_data_frames[["simA1"]]
simA2 <- modified_data_frames[["simA2"]]
simA3 <- modified_data_frames[["simA3"]]
simA4 <- modified_data_frames[["simA4"]]
simA5 <- modified_data_frames[["simA5"]]
simA6 <- modified_data_frames[["simA6"]]
simA7 <- modified_data_frames[["simA7"]]
simA8 <- modified_data_frames[["simA8"]]




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
same_sc_comp <- bind_rows(simB5, simA5)


same_sc_comp %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(!species %in% c("Ca", "Cj")) %>% 
  ggplot(aes(x = time,
             y = density,
             color = sc))+
  geom_line()+
  facet_grid(species~PP, 
             scales="free")
jhtuyeri 


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

