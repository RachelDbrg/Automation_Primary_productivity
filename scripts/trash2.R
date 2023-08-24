
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
