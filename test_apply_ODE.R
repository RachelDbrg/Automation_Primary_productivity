# Load the necessary packages
library(tidyverse)
library(deSolve)

# Function to calculate the parameter based on PP
calculate_parameter <- function(pp) {
  parameter <- 2 * pp  # Replace this with your actual calculation for the parameter
  return(parameter)
}

# Function to generate the data frame with PP values and calculated parameter
generate_parameter_dataframe <- function() {
  # Create a vector with "PP" values from 0 to 10 with incrementation of 0.1
  pp_values <- seq(0, 1, by = 0.1)
  
  # Calculate the parameter for each "PP" value
  parameters <- map_dbl(pp_values, calculate_parameter)
  
  # Create a data frame with "PP" and "Parameter" columns
  df_parameter_values <- tibble(PP = pp_values, Parameter = parameters)
  
  return(df_parameter_values)
}

# Call the function to get the data frame with PP values and calculated parameter
result_df <- generate_parameter_dataframe()



# Function to solve the ODE based on the parameter
ode_solver <- function(parameter) {
  # ODE function - Simple exponential growth ODE
  your_ode_function <- function(time, state, parameter) {
    with(as.list(c(state, parameter)), {
      dx <- parameter * x
      return(list(dx))
    })
  }
  
  # Initial conditions
  y0 <- c(x = 1)
  
  # Time points for integration
  times <- seq(0, 1, by = 0.1)
  
  # Solve the ODE
  ode_result <- ode(y = y0, times = times, func = your_ode_function, parms = parameter)
  return(ode_result)
}


# Use map2 to apply the ode_solver function to each set of parameters
ode_results_list <- map(result_df$Parameter, ode_solver)

# Plotting the ODE results for illustration
plot_results <- function(ode_result) {
  ggplot(as.data.frame(ode_result), aes(x = time, y = x)) +
    geom_line() +
    labs(title = "ODE Results", x = "Time", y = "Value") +
    theme_minimal()
}

# Plotting the ODE results for each set of parameters
plots <- lapply(ode_results_list, plot_results)
print(plots)
