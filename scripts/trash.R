source("Auto_gen_PP.R")

# Call the parameters that are assumed to not depend on PP
source("Static_vegetation_parameters.R")

source("Parametres_animaux_deers.R")

source("Evolution_vegetation.R")

source("equations_deers.R")

source("intermediate_res.R")

# Call the function to get the data frame
# initialise the values of PP-dependent parameter
# for each PP values
result_df <- generate_parameter_dataframe()

test <- result_df %>% 
  group_by(PP) %>% 
  nest()



evol_vg(1,9)

ret <-  result_df %>% 
  map_df(~ evol_vg(1, result_df$PP))



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
  
  k_m = ((2-0.84)*PP + 0.84) * coef
  
  k_c= ((11.43-4.74)*PP +4.74) * coef
  
  chi_M = m_croiss * ((a_M * e_UM * kUpeak)/
                        (1+a_M * h_UM * kUpeak) - mu_M)^-1
  
  chi_C = c_croiss * 
    ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1
  
  t_pertub = 100            # years
  # #t_pertub = 0
  
  # Definition des moments suivant la perturbation
  t_low = t_pertub + 5           # years : temps pour atteindre le minimum de
  # biomasse apres une perturbation
  t_kpeak = t_low + (50 - 25*PP) # years :  temps pour atteindre le maximum de
  # biomasse apres une perturbation
  t_kstable = t_kpeak + 100      # years : temps pour atteindre une stabilité de
  
  return(c(u_croiss, kUpeak, kUstable, w_0, kWpeak, kWstable,
           k_m, k_c, chi_M, chi_C, t_low, t_kpeak,t_kstable))
}


# Function to generate the data frame with PP values and calculated parameters

generate_parameter_dataframe <- function() {
  
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
           "kWstable" = V6,
           "k_m" = V7,
           "k_c" = V8,
           "chi_M" = V9,
           "chi_C" = V10,
           "t_low" = V11,
           "t_kpeak" = V12,
           "t_kstable" = V13) %>%
    select(PP, everything())
  
  return(df_parameter_values)
}




# Sample ODE function based on the provided parameters
ode_function <- function(time, state, params) {
  with(as.list(c(state, params)), {
    du_croiss <- y[2] * y[2] * exp(-y[4] * (time - y[2]))
    dw_0 <- y[2] * y[2] * exp(-y[2] * (time - y[2]))
    
    dU <- y[2] * y[1] - y[2] * y[2]
    dW <- y[2] * y[2] * (1 - y[2] / y[2]) - y[3] * y[2] *y[2]
    
    return(list(c(du_croiss, dU, dw_0, dW)))
  })
}



t0 = 0 # temps inital 
tfinal = 100 # temps final
# try simulation for shorter periods
# tfinal = 10 #years
# t_pertub = 100

# tfinal = 1 # temps final
tps <-seq (t0, tfinal, 0.25)

test_dataset <- result_df[1,]

sub_test <- test_dataset[, 1:4]

# test_dataset <- as_vector(test_dataset)

LVout <- ode(y = as.numeric(unlist(sub_test)),
             tps, ode_function, parms,
             method = "rk4") %>%
  as.data.frame()



# --------
LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    du_croiss <- kUpeak * u_croiss * exp(-kUstable * (Time - t_low))
    dw_0 <- kWpeak * w_0 * exp(-kWstable * (Time - t_low))
    
    dU <- k_m * w_0 - k_c * U
    dW <- chi_M * du_croiss * (1 - U / w_0) - chi_C * U * W
    
    return(list(c(du_croiss, dU, dw_0, dW,
                  kUpeak, kUstable,t_low,
                  kWpeak, w_0,
                  kWstable)))
  })
}

pars <- as.numeric(c(test_dataset <- result_df[1,]))

# pars  <- c(rIng   = 0.2,    # /day, rate of ingestion
#            rGrow  = 1.0,    # /day, growth rate of prey
#            rMort  = 0.2 ,   # /day, mortality rate of predator
#            assEff = 0.5,    # -, assimilation efficiency
#            K      = 10)     # mmol/m3, carrying capacity

yini  <- c(kUpeak = 1,
           kUstable = 3, t_low = 4,
           kWpeak =6, w_0 =7,
           kWstable = 8,k_m =9,
           k_c = 10, U = 11, W=2)
times <- seq(0, 200, by = 1)
out   <- ode(yini, times, LVmod, pars)
summary(out)


evol_vg(10, test$data, 1)


test <- result_df[1:3,]



# ==============


low_density_east <- low_density

# List files ending with "east"
east_files <- list.files(pattern = "east$")

# Iterate through each file
for (file_name in east_files) {
  # Read the data from the file
  data <- read.csv(file_name)  # You may need to adjust the read function based on your data format
  
  # Filter rows where time == 2000
  filtered_data <- subset(data, time == 2000)
  
  # Extract the NAME from the file name
  name <- gsub("_density.*", "", file_name)
  
  # Create an object with the filtered data
  assign(paste0("fd_", name), filtered_data)
}






