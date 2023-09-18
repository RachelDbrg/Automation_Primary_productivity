library(tidyverse)
library(readr)

# Import the file where only wolf within the clipped area are retained


wolves_within_eastern_area <- read_csv("~/wolves_within_eastern_area.csv")


# How many wolves were recorded in this area?

wolves_within_eastern_area %>% 
  group_by(id, year) %>% 
  count() %>% 
  View()
