compute_km <- function(PP){
  
  km = (1+(PP/10))*0.83
  km1 = (1+PP)*0.83
  km2 = (PP)*0.83
  km3 = (2*PP)*0.83
  ku_stable = 4*10^5
  ku = ku_stable *(1+PP/2)
  kM_courtois = 0.9*PP + 0.2
  km_actuel = (2-0.84)*PP + 0.84
  k_c_actuel = ((11.43-4.74)*PP +4.74)
  k_c_test = PP * 3 + 2
  # k_c_test_modif = k_c_test * 0.2
  k_c_test_modif = PP * 10 + 0.5
  tre = 5*PP + 0.2
  
  
  return(data.frame(PP,km, km1,km2, km3, ku, kM_courtois, km_actuel,k_c_actuel,k_c_test,k_c_test_modif,tre))
}

PP <- seq(0,1, 0.1)

compute_km(PP)



courtois <- readRDS(file = "~/Automation_Primary_productivity/Carrying_capacities/courtois.R")
courtois_west <- readRDS(file = "~/Automation_Primary_productivity/Carrying_capacities/courtois_west.R")
courtois_west_modif_kc <- readRDS(file = "~/Automation_Primary_productivity/Carrying_capacities/courtois_west_modif_kc.R")
regular <- readRDS(file = "~/Automation_Primary_productivity/Carrying_capacities/regular.R")


courtois <- courtois %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "courtois")


courtois_west <- courtois_west %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "courtois_west")

courtois_west_modif_kc <- courtois_west_modif_kc %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "courtois_west_modif_kc")


regular <- regular %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "regular")


test <- bind_rows(courtois, regular)
test_west <- bind_rows(courtois, courtois_west)
test_kc_west <- bind_rows(test_west, courtois_west_modif_kc)

all <- bind_rows(test, test_west)
all <- bind_rows(all, test_kc_west)



all %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time, y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free")



# ==============================================================================
# Dans tous ces scenarios, quelle est la relation entre PP et l'augmentation
# de la nourriture?

test_kc_west %>%
  group_by(PP) %>% 
  filter(time == 2000) %>% 
  ggplot(aes(x = PP,
             y = U,
             color = zone)) +
  geom_point()




test %>%
  group_by(PP) %>% 
  filter(time == 2000) %>% 
  ggplot(aes(x = PP,
             y = M_tot,
             color = zone)) +
  geom_point()



test %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time, y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free")



test_kc_west %>% 
  filter(time == 2000) %>% 
  # filter(zone=="west") %>% 
  # filter(proies_tot <= 5) %>%
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
             color = zone))+
  geom_point()


test %>% 
  filter(time == 2000) %>% 
  ggplot(aes(x = PP,
             y = M_tot,
             colors = zone)) +
  geom_point()



test_west %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(time >= 50 & time <= 300) %>% 
  ggplot(aes(x = time,
             y= density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales="free")



test_kc_west %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(time >= 50 & time <= 300) %>% 
  ggplot(aes(x = time,
             y= density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales="free")




# ========================
# Combien de temps pour se stabiliser?
test %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  group_by(species) %>% 
  mutate(delta = lag(density) - density,
         stable = ifelse(delta != "0", "NO", "YES")) %>% 
  View()
