# Load the necessary packages
library(tidyverse)
library(deSolve)


# Function to calculate the parameter based on PP
calculate_parameter <- function(pp) {
  parameter <- 2 * pp  # Replace this with your actual calculation for the parameter
  parameter1 <- 3 * pp  # Replace this with your actual calculation for the parameter
  return(list(par1 = parameter, par2 = parameter1))
}

# Function to generate the data frame with PP values and calculated parameter
generate_parameter_dataframe <- function() {
  # Create a vector with "PP" values from 0 to 10 with incrementation of 0.1
  pp_values <- seq(0, 1, by = 0.1)
  
  # Calculate the parameter for each "PP" value
  parameters <- pmap_df(list(pp = pp_values), calculate_parameter)
  
  # Create a data frame with "PP" and "Parameter" columns
  # df_parameter_values <- tibble(PP = pp_values, Parameter = parameters)
  
  df_parameter_values <- data.frame(PP = pp_values, Parameter = parameters)
  
  # return(df_parameter_values)
  return(df_parameter_values)
}

# Call the function to get the data frame with PP values and calculated parameter
result_df <- generate_parameter_dataframe()


# Function to solve the ODE based on the parameter
ode_solver <- function(parameter, parameter1) {
  # ODE function - Simple exponential growth ODE
  your_ode_function <- function(time, state, parameter) {
    with(as.list(c(state, parameter)), {
      dx <- parameter * parameter1 * x
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
ode_results_list <- map2(result_df$Parameter.par1, result_df$Parameter.par2, ode_solver) %>% 
  as.data.frame()





