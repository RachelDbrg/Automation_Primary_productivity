rm(list = ls(all.names = TRUE))

# Load the df containing all the simulations 
all_simulations <- readRDS ("all_simulations.R")

# Subset the data
# PP values that we want to look at
subset_PP <- seq(1,10,1)

subdata <- all_simulations %>% 
  filter(PP %in% c(seq(1,10,1)))
   