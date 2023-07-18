# Load the tidyverse package
library(tidyverse)

# Function to calculate the parameters based on PP
calculate_parameters <- function(PP) {
  
  u_croiss <-  (1+4*PP)*30000
  
  kUpeak  <-  (1 + PP/2)*kUpeak_0
  
  kUstable <- (1+(PP/2))* kUstable_0 * 2.4
  
  w_0 <-  (1+4*PP)*300      # kg/(km² an), taux de recroissance initial
  
  kWpeak <- (1+PP/2)*kWpeak_0 # kg/km^2, pour t=t_kpeak
  
  kWstable <- (1 + PP/2)*kWstable_0 # kg/km^2, pour t=t_kstable
  
  return(c(u_croiss, kUpeak, kUstable, w_0, kWpeak, kWstable))
}

# Create a vector with "PP" values from 0 to 10 with incrementation of 0.1
pp_values <- seq(0, 10, by = 0.1)

# Use map_dfc() to calculate the parameters for each "PP" value and combine them into a data frame
parameter_data <- map(pp_values, calculate_parameters)


# Add the "PP" column to the data frame
# df_parameter_values <- bind_cols(PP = pp_values, df_parameter_values)
df_parameter_values <- as.data.frame(do.call(rbind, parameter_data))


# Add the PP column
df_parameter_values$PP <- pp_values

# Rename the columns and reorder with "PP" column first
df_parameter_values <- df_parameter_values %>% 
  rename("u_croiss" = V1,
         "kUpeak" = V2,
         "kUstable" = V3,
         "w_0" = V4,
         "kWpeak" = V5,
         "kWstable" = V6) %>%
  select(PP, everything())

