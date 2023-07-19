# Load the necessary packages
library(tidyverse)

# Sample multi-output function
multi_output_function <- function(x) {
  output1 <- x^2
  output2 <- x + 10
  return(list(Output1 = output1, Output2 = output2))
}

multi_output_function(0.1)

# Create a vector of input values
input_values <- c(1, 2, 3, 4, 5)

# Apply the multi-output function using pmap
result_list <- pmap(list(x = input_values), multi_output_function)

# Convert the list of results into a data frame
result_df <- as.data.frame(result_list)

# Print the result data frame
print(result_df)
