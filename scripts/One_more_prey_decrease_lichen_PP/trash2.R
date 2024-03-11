
# Define a sequence of PP values
PP_values <- seq(0, 1, by = 0.1)  # Example range from 0 to 1 with step 0.1

# Initialize an empty list to store results
biomass_results <- list()

# Apply the equation for each PP value and store results
for (PP in PP_values) {
  deciduous_biomass <- 30000 * (1 + 4 * PP)
  biomass_results[[as.character(PP)]] <- deciduous_biomass
}

# Print the results
for (PP in PP_values) {
  cat("PP:", PP, "Biomass:", biomass_results[[as.character(PP)]], "\n")
}

# Load the ggplot2 library
library(ggplot2)

# Define a data frame from the results
results_df <- data.frame(PP = as.numeric(names(biomass_results)),
                         Biomass = unlist(biomass_results))

# Create a scatter plot
ggplot(data = results_df, aes(x = PP, y = Biomass)) +
  geom_point() +
  geom_line() +
  labs(title = "Deciduous Biomass as a Function of PP",
       x = "PP Value",
       y = "Deciduous Biomass") +
  theme_minimal()



# ========
East_phi_1_test_km <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_phi_1_test_km.R")



East_phi_1_test_km_init <- East_phi_1_test_km %>% 
  pull(data) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_phi_1_test_km")


East_phi_1_test_km <- East_phi_1_test_km %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "East_phi_1_test_km")

East_phi_1_test_km %>%
  mutate(tot = Mj + Ma) %>%
  filter(time %in% c(10, 75)) %>% View()


East_phi_1_test_km %>% 
  mutate(diff = k_m - Ma+Mj,
         diff2 = k_m - Ma,
         diff3 = k_c - Ca) %>% 
  pivot_longer(cols = c(2:10, k_c, diff, diff2, diff3),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("Ca", "Cj", "U", "k_c", "diff", "diff2", "diff3")) %>%
  ggplot(aes(x = time, y = density,
             color = PP))+
  geom_line()+
  facet_grid(species~PP, scales = "free") +
  theme_minimal()


East_phi_1_test_km %>% 
  # filter(time == 500) %>% View()
  pivot_longer(cols = c(2:10, k_c),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("Ca", "Cj", "U", "k_c")) %>%
  ggplot(aes(x = time, y = density,
             color = PP))+
  geom_line()+
  facet_grid(species~PP, scales = "free") +
  theme_minimal()
  
