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
all_combinaison <- bind_rows(all_combinaison, Moose)


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


all_combinaison %>% 
  filter(time == 2000) %>% 
  rename(Deer_adult = Ca,
         Deer_young = Cj,
         Caribou_adult = Na,
         Caribou_young = Nj,
         Moose_adult = Ma,
         Moose_young = Mj,
         Wolf = P) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  # filter(species == "Ca") %>% 
  ggplot(aes(x = PP, y = density,
             color = zone))+
  geom_point()+
  facet_wrap(~species, scales = "free") + 
  theme_minimal()


custom_order <- c("West", "East", "moose_cerf",
                  "caribou_cerf", "Moose", "Cerf")

# Créer le facteur en spécifiant l'ordre des niveaux
all_combinaison$zone = as.factor(all_combinaison$zone)

all_combinaison$zone <- factor(all_combinaison$zone, levels = custom_order)

# Get the values
all_combinaison %>% 
  filter(time == 2000) %>% 
  rename(Deer_adult = Ca,
         Deer_young = Cj,
         Caribou_adult = Na,
         Caribou_young = Nj,
         Moose_adult = Ma,
         Moose_young = Mj,
         Wolf = P) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  # group_by(species, zone) %>% 
  ggplot(aes(x = as.factor(zone), y = density,
             color = PP))+
  geom_point()+
  geom_boxplot()+
  facet_wrap(~species, scales = "free") + 
  theme_minimal()+
  theme(axis.title.x=element_blank())


# Compute the %of change between final densities at PP = 0 et PP = 1, per species
# per scenario
all_combinaison %>% 
  filter(time == 2000) %>% 
  rename(Deer_adult = Ca,
         Deer_young = Cj,
         Caribou_adult = Na,
         Caribou_young = Nj,
         Moose_adult = Ma,
         Moose_young = Mj,
         Wolf = P) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(PP %in% c("0", "1")) %>% 
  group_by(species, zone) %>%
  mutate(percent_change = density*100/lag(density),
         txt_dec = paste("decrease of ", 100 - round(percent_change,2), "%"),
         txt_inc = paste("increase of ", round(percent_change,2) - 100, "%"),
         variation_sens = if_else(percent_change >= 100,txt_inc, txt_dec)) %>% 
  filter(zone %in% c("East", "West")) %>% View()
  filter(variation_sens != "NA") %>% 
  
# filter(species == "Wolf") %>% View()
  select(species, zone, percent_change, variation_sens) %>% 
  


# Get the mean values per species
all_combinaison %>% 
  filter(time == 2000) %>% 
  rename(Deer_adult = Ca,
         Deer_young = Cj,
         Caribou_adult = Na,
         Caribou_young = Nj,
         Moose_adult = Ma,
         Moose_young = Mj,
         Wolf = P) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>%
  filter(species %in% c("Caribou_adult", "Caribou_young")) %>% 
  group_by(species, zone) %>% 
  summarise(mean_density = mean(density))



# ==============================================================================

# For caribou only, for every PP comparison
# Compute the %of change between final densities at PP = 0 et PP = 0.1, 
# 0.2, 0.3..., per scenario

all_combinaison %>% 
  filter(time == 2000) %>% 
  rename(Deer_adult = Ca,
         Deer_young = Cj,
         Caribou_adult = Na,
         Caribou_young = Nj,
         Moose_adult = Ma,
         Moose_young = Mj,
         Wolf = P) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  # filter(PP %in% c("0", "1")) %>% 
  filter(species %in% c("Caribou_adult", "Caribou_young")) %>% 
  group_by(species, zone) %>%
  mutate(test = lag(density),
         diff = density -lag(density),
         percent_change = density*100/lag(density),
         txt_dec = paste("decrease of ", 100 - round(percent_change,2), "%"),
         txt_inc = paste("increase of ", round(percent_change,2) - 100, "%"),
         variation_sens = if_else(percent_change >= 100,txt_inc, txt_dec)) %>%
  # filter(species == "Wolf") %>% View()
  select(species, zone, PP, density, diff, percent_change, variation_sens) %>% 
  filter(variation_sens != "NA") %>% 
  ggplot(aes(x = PP)) +
  geom_point(aes(y = diff, color = diff > 0))+
  # geom_point(aes(y = density))+
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Differences of densities, comparing productivity for caribou",
       subtitle = "red points are positive difference, ie increase in the density while blue points are the opposite") +
  facet_grid(species~zone, scales = "free")

  
