East <- readRDS(file = "~/Automation_Primary_productivity/Species_composition_simulations/East.R")
West <- readRDS(file = "~/Automation_Primary_productivity/Species_composition_simulations/West.R")
moose_cerf <- readRDS(file = "~/Automation_Primary_productivity/Species_composition_simulations/moose_cerf.R")
caribou_cerf <- readRDS(file = "~/Automation_Primary_productivity/Species_composition_simulations/Caribou_cerf.R")
Moose <- readRDS(file = "~/Automation_Primary_productivity/Species_composition_simulations/Moose.R")
Cerf <- readRDS(file = "~/Automation_Primary_productivity/Species_composition_simulations/Cerf.R")


East <- East %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East")

West <- West %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "West")

moose_cerf <- moose_cerf %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "moose_cerf")

caribou_cerf <- caribou_cerf %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "caribou_cerf")

Moose <- Moose %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "Moose")

Cerf <- Cerf %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "Cerf")


all_combinaison <- bind_rows(East, West)
all_combinaison <- bind_rows(all_combinaison, moose_cerf)
all_combinaison <- bind_rows(all_combinaison, caribou_cerf)
all_combinaison <- bind_rows(all_combinaison, Cerf)
all_combinaison <- bind_rows(all_combinaison, caribou_cerf)


all_combinaison %>%
  filter(PP %in% c("0", "1"),
         time <= 500) %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time, y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free") + 
  theme_minimal()
