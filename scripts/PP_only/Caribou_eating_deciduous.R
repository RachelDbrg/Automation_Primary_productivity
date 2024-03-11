# first_attempt <- readRDS(file = "~/Automation_Primary_productivity/Species_diet/first_attempt.R")
west <- readRDS(file = "~/Automation_Primary_productivity/Species_diet/west.R")
east <- readRDS(file = "~/Automation_Primary_productivity/Species_diet/east.R")
west_caribou_deciduous <- readRDS(file = "~/Automation_Primary_productivity/Species_diet/west_caribou_deciduous.R")
east_caribou_deciduous <- readRDS(file = "~/Automation_Primary_productivity/Species_diet/east_caribou_deciduous.R")



library(tidyverse)

# first_attempt <- first_attempt %>% 
#   pull(outputs) %>%   
#   map_dfr(as.data.frame) %>% 
#   mutate_all(as.numeric) %>% 
#   mutate(zone = "first_attempt")

west <- west %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "west")

east <- east %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "east")

west_caribou_deciduous <- west_caribou_deciduous %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "west_caribou_deciduous")

east_caribou_deciduous <- east_caribou_deciduous %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "east_caribou_deciduous")

all <- bind_rows(east, west)
all <- bind_rows(all, west_caribou_deciduous)
all <- bind_rows(all, east_caribou_deciduous)



all %>%
  filter(PP %in% c("0", "1"),
         time <= 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time, y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free") + 
  theme_minimal()


# Final densities
all %>%
  filter(time == 2000,
         grepl("east", zone)) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  group_by(species, PP) %>% 
  # filter(species == "Na") %>% 
  mutate(diff = density-lag(density),
         percent_change = density*100/lag(density),
         txt_dec = paste("decrease of ", 100 - round(percent_change,2), "%"),
         txt_inc = paste("increase of ", round(percent_change,2) - 100, "%"),
         variation_sens = if_else(percent_change >= 100,txt_inc, txt_dec)) %>% 
  select(PP, density, diff, species, percent_change, variation_sens,zone) %>% 
  filter(diff != "NA") %>% 
  ggplot(aes(x = PP)) +
  geom_point(aes(y = diff, color = diff > 0))+
  # geom_point(aes(y = density))+
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Differences of densities, comparing productivity for caribou",
       subtitle = "red points are positive difference, ie increase in the density while blue points are the opposite") +
  facet_grid(species~zone, scales = "free")





kM <- kM %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "kM")

kM_data <- kM %>% 
  pull(data) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "kM")


all <- bind_rows(first_attempt, west)

all %>%
  filter(PP %in% c("0", "1"),
         time <= 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time, y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free") + 
  theme_minimal()


west %>%
  ggplot(aes(x = U,
             y = V49))+
  geom_point()



east_nouveau_km <- readRDS(file = "~/Automation_Primary_productivity/Carrying_capacities/east_nouveau_km.R")


east_nouveau_km <- east_nouveau_km %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "east_nouveau_km")

km_compar <- bind_rows(east, east_nouveau_km)




# ==============================================================================
kM <- readRDS(file = "~/Automation_Primary_productivity/Species_diet/kM.R")


# Final densities
km_compar %>%
  filter(PP %in% c("0", "1")) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time , y = density,
             color = zone))+
  geom_line()+
  facet_grid(species~PP, scales = "free") + 
  theme_minimal()

