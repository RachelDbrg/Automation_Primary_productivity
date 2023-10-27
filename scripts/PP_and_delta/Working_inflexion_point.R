library(tidyverse)
theme_set(theme_minimal())

# ==============================================================================
# Load all the datafiles for the eastern simulations

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
simB9 <- modified_data_frames[["simB9"]]
simB10 <- modified_data_frames[["simB10"]]
simB11 <- modified_data_frames[["simB11"]]
simB12 <- modified_data_frames[["simB12"]]
simB13 <- modified_data_frames[["simB13"]]
simB14 <- modified_data_frames[["simB14"]]
simB15 <- modified_data_frames[["simB15"]]
simB16 <- modified_data_frames[["simB16"]]
simB17 <- modified_data_frames[["simB17"]]
simB18 <- modified_data_frames[["simB18"]]
simB19 <- modified_data_frames[["simB19"]]



test_west <- do.call("rbind", mget(ls(pattern = "^simB")))

saveRDS(test_west, file = "test_west.rds")



test_west <- test_west %>% 
  mutate(across(!sc, as.numeric)) %>% 
  filter(time == 2000) %>% 
  mutate(zone = "west")




# Ecart densite loups avec Messier
test_west %>% 
  filter(proies_tot <= 10) %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(densite_messier_coeffd = ((58.7*(Ma+Mj-0.03))/(0.76+(Ma+Mj))/1000) + ((58.7*(Na+Nj-0.03))/(0.76+(Na+Nj))/1000)*0.3
         + ((58.7*(Ca+Cj-0.03))/(0.76+(Ca+Cj))/1000)*0.2) %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model", shape=sc))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
  geom_point(aes (y = densite_messier_coeffd, color = "densite_messier_coeffd"))




test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()+
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       color = "")+
  theme_minimal()


# ==============================================================================
test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()+
  facet_wrap(~predation_rate)+
  labs(title = "Predation rates of prey species. in function of the system's complexity",
       subtitle = "The total prey density",
       x = "Total prey density (ind/km2)",
       y = "Predation rate (number of prey killed / total prey density)",
       color = "Zone")+
  theme_minimal()

# ==============================================================================


test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
    filter(sc %in% c("simB6",
                     "simB10",
                     "simB11",
                     "simB12",
                     "simB19")) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate == "taux_predation_caribou") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = sc))+
  geom_point()


test_west_values <- test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot)


rank_caribou <- which.max(test_west_values$taux_predation_caribou)

test_west_values[rank_caribou,]
# proies_tot = 0.272965 
# taux_predation_caribou = 0.01511575
# Densite orignal= 0.1573003  
# Densite cerf= 0.4825425     
# Densite caribou = 0.1292664 
# Loup = 0.00329336    


rank_moose <- which.max(test_west_values$taux_predation_moose)
test_west_values[rank_moose,]
# proies_tot  =  1.555716 
# taux_predation_moose = 0.1543275                  
# Densite orignal= 0.1579987   
# Densite cerf= 1.363284     
# Densite caribou = 1.23566   
# Loup = 0.05654266 

rank_deer <- which.max(test_west_values$taux_predation_cerf)
test_west_values[rank_deer,]
# proies_tot  =  7.630208 
# taux_predation_cerf = 0.2411405         
# Densite orignal= 0.1767452  
# Densite cerf= 12.02158        
# Densite caribou = 5.172868  
# Loup = 0.1192471  



# ==============================================================================
# ==============================================================================

# Load all the datafiles for the eastern simulations

# Directory where your RDS files are located
directory <- "~/Automation_Primary_Productivity/Messier_figure/After_correction/"

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

library(tidyverse)
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
simA9 <- modified_data_frames[["simA9"]]
simA10 <- modified_data_frames[["simA10"]]
simA11 <- modified_data_frames[["simA11"]]
simA12 <- modified_data_frames[["simA12"]]
simA13 <- modified_data_frames[["simA13"]]
simA14 <- modified_data_frames[["simA14"]]
simA15 <- modified_data_frames[["simA15"]]


test_est <- do.call("rbind", mget(ls(pattern = "^simA")))

saveRDS(test_est, file = "test_est.RDS")


test_est %>% 
  filter(time ==2000) %>% 
  mutate(across(!sc, as.numeric)) %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Ma") %>% 
  ggplot(aes(x = time,
             y = density))+
  geom_line()+
  facet_grid(species~PP, 
             scales="free")



test_est <- test_est %>% 
  mutate(across(!sc, as.numeric)) %>% 
  filter(time == 2000) %>% 
  mutate(zone = "est")


test_est %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 5) %>%
  # filter(sc %in% c("simA3", "simA4", "simA9", "simA10")) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate == "taux_predation_caribou") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = sc))+
  geom_point()


test_est_values <- test_est %>% 
  # filter(zone=="west") %>% 
  # filter(proies_tot <= 10) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot)


rank_caribou <- which.max(test_est_values$taux_predation_caribou)

test_est_values[rank_caribou,]
# proies_tot = 0.3575076  
# taux_predation_caribou = 0.01420073         
# Densite orignal= 0.1513651  
# Densite cerf= 0  
# Densite caribou = 0.3120981      
# Loup = 0.006451505   


rank_moose <- which.max(test_est_values$taux_predation_moose)
test_est_values[rank_moose,]
# proies_tot  =  1.208429   
# taux_predation_moose = 0.2112282                          
# Densite orignal= 0.1543659   
# Densite cerf= 0     
# Densite caribou = 1.16212       
# Loup = 0.05232203   



both_species <- bind_rows(test_est,
                          test_west)


both_species %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  # filter(predation_rate == "taux_predation_caribou") %>% 
  mutate(predation_rate = recode (predation_rate,
                                  taux_predation_caribou = "Caribou",
                                  taux_predation_moose = "Moose",
                                  taux_predation_cerf = "Deer"),
         Zone = recode(zone, est = 'East', west = 'West')) %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = Zone))+
  geom_point(alpha = 0.5)+
  # geom_smooth()+
  facet_wrap(~predation_rate, scales = "free")+
  labs(title = "Fluctuation in predation rate with total prey density",
       # subtitle = "The dash line represent the time when perturbation occurs",
       color= "Zone",
       x = "Total prey density (ind/km2)",
       y = "Predation rate (number of prey eaten / total available prey)")



both_species %>% 
  filter(proies_tot <= 2) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate == "taux_predation_caribou") %>%
  mutate(Zone = recode(zone, est = 'East', west = 'West')) %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = Zone))+
  geom_point(alpha = 0.5)+
  # geom_smooth()+
  # facet_wrap(~predation_rate, scales = "free")+
  labs(title = "Fluctuation in predation rate with total prey density",
       subtitle = "Zoom on the caribou, when total prey density is <= 2 ind/km2",
       color= "Zone",
       x = "Total prey density (ind/km2)",
       y = "Predation rate (number of prey eaten / total available prey)")




# ==============================================================================
both_sp_values <- both_species %>% 
  filter(zone == "est") %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot)


rank_caribou <- which.max(both_sp_values$taux_predation_caribou)

both_sp_values[rank_caribou,]


# ==============================================================================
test_west <- readRDS(file = "test_west.rds")
test_est <- readRDS(file = "test_est.rds")

library(tidyverse)

test_est <- test_est %>% 
  mutate(zone = "est") %>% 
  filter(time == 2000)

test_west <- test_west %>% 
  mutate(zone = "west") %>% 
  filter(time == 2000)

both_species <- bind_rows(test_est,
                          test_west)


both_species %>% 
  filter(proies_tot <= 2) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate == "taux_predation_caribou") %>%
  mutate(Zone = recode(zone, est = 'East', west = 'West')) %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = Zone))+
  geom_point(alpha = 0.5)+
  # geom_smooth()+
  # facet_wrap(~predation_rate, scales = "free")+
  labs(title = "Fluctuation in predation rate with total prey density",
       subtitle = "Zoom on the caribou, when total prey density is <= 2 ind/km2",
       color= "Zone",
       x = "Total prey density (ind/km2)",
       y = "Predation rate (number of prey eaten / total available prey)")
  



bool_Messier <- both_species %>% 
  filter(proies_tot <= 2,
         zone == "est") %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate == "taux_predation_caribou") %>%
  mutate(loup_Messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000,
         min_30_prct_Messier = 0.8*loup_Messier,
         max_30_prct_Messier = 1.2*loup_Messier,
         Messier_accurate = case_when((P >= min_30_prct_Messier & P <= max_30_prct_Messier) ~ "20prct", .default = "other"),
         diff_P = case_when(P >= loup_Messier ~"plus_grand", .default = "plus_petit") )
  # mutate(Zone = recode(zone, est = 'East', west = 'West')) %>% 
  


bool_Messier %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = diff_P))+
  geom_point()+
  labs(title = "Fluctuation in predation rate with total prey density",
       subtitle = "Zoom on the caribou, when total prey density is <= 2 ind/km2",
       color= "Zone",
       x = "Total prey density (ind/km2)",
       y = "Predation rate (number of prey eaten / total available prey)")
