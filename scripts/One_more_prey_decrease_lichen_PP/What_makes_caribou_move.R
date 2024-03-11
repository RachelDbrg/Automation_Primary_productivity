# What makes caribou population densities change?

# Load the df
East <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East.R")
East_Vinit_mlt_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Vinit_mlt_100.R")
East_Vinit_mlt_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Vinit_mlt_10.R")
East_Vinit_div_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Vinit_div_10.R")
East_Vinit_div_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Vinit_div_100.R")
East_Pinit_div_5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Pinit_div_5.R")
East_Pinit_div_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Pinit_div_10.R")
East_Pinit_mlt_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Pinit_mlt_10.R")
East_Pinit_mlt_5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Pinit_mlt_5.R")
East_Minit_mlt_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Minit_mlt_100.R")
East_Minit_mlt_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Minit_mlt_10.R")
East_Minit_div_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Minit_div_10.R")
East_Ninit_mlt_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Ninit_mlt_10.R")
East_Ninit_mlt_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Ninit_mlt_100.R")
East_Ninit_div_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_Ninit_div_100.R")
East_phi_1 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_phi_1.R")
East_phi_1_P_init_mlt_5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_phi_1_P_init_mlt_5.R")



West <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West.R")
West_Vinit_div_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Vinit_div_10.R")
West_Vinit_div_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Vinit_div_100.R")
West_Vinit_mlt_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Vinit_mlt_10.R")
West_Vinit_mlt_100 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Vinit_mlt_100.R")
West_Pinit_div_5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Pinit_div_5.R")
West_Pinit_div_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Pinit_div_10.R")
West_Pinit_mlt_10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Pinit_mlt_10.R")
West_Pinit_mlt_5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/West_Pinit_mlt_5.R")


East <- East %>% 
  pull(data) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East")

East_Vinit_mlt_100 <- East_Vinit_mlt_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Vinit_mlt_100")

East_Vinit_mlt_10 <- East_Vinit_mlt_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Vinit_mlt_10")

East_Vinit_div_10 <- East_Vinit_div_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Vinit_div_10")

East_Vinit_div_100 <- East_Vinit_div_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Vinit_div_100")

East_Pinit_div_5 <- East_Pinit_div_5 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Pinit_div_5")

East_Pinit_div_10 <- East_Pinit_div_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Pinit_div_10")

East_Pinit_mlt_10 <- East_Pinit_mlt_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Pinit_mlt_10")

East_Pinit_mlt_5 <- East_Pinit_mlt_5 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Pinit_mlt_5")

East_Minit_mlt_10 <- East_Minit_mlt_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Minit_mlt_10")

East_Minit_mlt_100 <- East_Minit_mlt_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Minit_mlt_100")

East_Minit_div_10 <- East_Minit_div_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Minit_div_10")

East_Ninit_mlt_10 <- East_Ninit_mlt_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Ninit_mlt_10")

East_Ninit_mlt_100 <- East_Ninit_mlt_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Ninit_mlt_100")

East_Ninit_div_100 <- East_Ninit_div_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_Ninit_div_100")

East_phi_1 <- East_phi_1 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_phi_1")

East_phi_1_P_init_mlt_5 <- East_phi_1_P_init_mlt_5 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_phi_1_P_init_mlt_5")




East_df <- do.call("rbind", list(East, East_Vinit_mlt_100, East_Vinit_mlt_10,
                                 East_Vinit_div_10, East_Vinit_div_100, 
                                 East_Pinit_div_5, East_Pinit_div_10, 
                                 East_Pinit_mlt_10, East_Pinit_mlt_5,
                                 East_Ninit_div_100, East_Ninit_mlt_100,
                                 East_Ninit_mlt_10, East_Minit_div_10,
                                 East_Minit_mlt_100, East_Minit_mlt_10,
                                 East_phi_1, East_phi_1_P_init_mlt_5))


West <- West %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West")

West_Vinit_mlt_100 <- West_Vinit_mlt_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Vinit_mlt_100")

West_Vinit_mlt_10 <- West_Vinit_mlt_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Vinit_mlt_10")

West_Vinit_div_10 <- West_Vinit_div_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Vinit_div_10")

West_Vinit_div_100 <- West_Vinit_div_100 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Vinit_div_100")

West_Pinit_div_5 <- West_Pinit_div_5 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Pinit_div_5")

West_Pinit_div_10 <- West_Pinit_div_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Pinit_div_10")

West_Pinit_mlt_10 <- West_Pinit_mlt_10 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Pinit_mlt_10")

West_Pinit_mlt_5 <- West_Pinit_mlt_5 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West_Pinit_mlt_5")



# ==============================================================================
# Plot the outcomes of the different runs

East_df %>% 
  filter(PP %in% c(0,1)) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Na") %>% 
  filter(time >= 500) %>% 
  ggplot(aes(x = time, y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free") + 
  theme_minimal()


East_df %>%
  filter(time == 2000) %>% 
  select(c(2:8))
  View()
