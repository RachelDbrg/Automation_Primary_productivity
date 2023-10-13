
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


test_west <- do.call("rbind", mget(ls(pattern = "^simB")))


test_west <- test_west %>% 
  filter(time == 2000)



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
# Loup = 0.05923227   


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



test_est %>% 
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


# =========== Test of productivity effect ======================================

simA5 %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Ma") %>% 
  ggplot(aes(x = time,
             y = density))+
  geom_line()+
  facet_grid(PP~species, scales="free")

simA1 <- simA1 %>% 
  mutate(sim = "A1")

simA2 <- simA2 %>% 
  mutate(sim = "A2")

test_est_test <- bind_rows(simA1, simA2)

test_est_test %>% 
  filter(time == 2000) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density,
             color=factor(sim))+
           geom_point()+
           facet_wrap(~species, scales="free"))
         
         simA8 %>% 
           filter(time == 2000) %>% 
           mutate_all(as.numeric) %>% 
           pivot_longer(c(2:10),
                        names_to = "species",
                        values_to = "density") %>% 
           ggplot(aes(x = PP,
                      y = density))+
           geom_point()+
           facet_wrap(~species, scales="free")
         
         
         simB5 %>% 
           mutate_all(as.numeric) %>% 
           pivot_longer(c(2:10),
                        names_to = "species",
                        values_to = "density") %>% 
           ggplot(aes(x = time,
                      y = density))+
           geom_point()+
           facet_wrap(~species, scales="free")