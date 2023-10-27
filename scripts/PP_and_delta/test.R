sim10 <- readRDS("~/Automation_Primary_productivity/Messier_figure/After_correction/delta_PP.R")


sim10init <- sim10 %>% 
  pull(data) %>% 
  map_dfr(as.data.frame)


sim1_out <- sim10 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) 
  
  
  
  mutate(zone = "est",
         delta = 0.1,
         phi = 0.3,
         sc = "sim1") %>% 
  filter(time == 2000)





my_function <- function(...) {
  args <- list(...)
  
  PP <- scenarios$PP
  delta <- scenarios$delta
  
  test <- PP + delta
  
  return(test)
  
}


scenarios <- expand.grid(
  PP = seq(0.9, 1, 0.1),
  delta = seq(1, 1.1, 0.1))


calculate_parameters <- function(PP) {
  
  ## Vegetation
  # Deciduous growth rate 
  u_croiss <-  (1+4*PP)*30000 
  
  # Max deciduous carrying capacity 
  kUpeak  <-  (1 + PP/2)*kUpeak_0
  
  # Stable deciduous carrying capacity 
  kUstable <- (1+(PP/2))* kUstable_0 * 2.4
  
  # Shrubs growth rate 
  w_0 <-  (1+4*PP)*300      # kg/(km² an), taux de recroissance initial
  
  # Max shrubs carrying capacity 
  kWpeak <- (1+PP/2)*kWpeak_0 # kg/km^2, pour t=t_kpeak
  
  # Stable shrubs carrying capacity 
  kWstable <- (1 + PP/2)*kWstable_0 # kg/km^2, pour t=t_kstable
  
  # --------------------------------------------------------
  
  ## Animals
  # Moose carrying capacity
  k_m = ((2-0.84)*PP + 0.84) * delta
  
  # Devrait être exprimée en fonction de la quantité de feuillus!!!
  
  # Moose growth rate
  chi_M = m_croiss * ((a_M * e_UM * kUpeak)/
                        (1+a_M * h_UM * kUpeak) - mu_M)^-1
  
  # Deer carrying capacity
  k_c= ((11.43-4.74)*PP +4.74) * delta
  
  # Deer growth rate
  chi_C = c_croiss * 
    ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1
  
  # --------------------------------------------------------
  
  t_perturb = 100            # years
  # #t_pertub = 0
  
  # Definition des moments suivant la perturbation
  t_low = t_perturb + 5           # years : temps pour atteindre le minimum de
  # biomasse apres une perturbation
  t_kpeak = t_low + (50 - 25*PP) # years :  temps pour atteindre le maximum de
  # biomasse apres une perturbation
  t_kstable = t_kpeak + 100      # years : temps pour atteindre une stabilité de
  
  kUcoeff1 = (kUstable - kUpeak)/(t_kstable - t_kpeak)
  kUcoeff2 = kUpeak - (kUstable - kUpeak)/(t_kstable - t_kpeak) * t_kpeak
  kWcoeff1 = (kWstable - kWpeak)/(t_kstable - t_kpeak)
  kWcoeff2 = kWpeak - (kWstable - kWpeak)/(t_kstable - t_kpeak) * t_kpeak
  
  return(c(u_croiss, kUpeak, kUstable, w_0, kWpeak, kWstable,
           k_m, k_c, chi_M, chi_C, t_low, t_kpeak,t_kstable,
           kUcoeff1, kUcoeff2, kWcoeff1, kWcoeff2, t_perturb))
}




generate_parameter_dataframe <- function() {
  
  # Create a vector with "PP" values from 0 to 10 with incrementation of 0.1
  # pp_values <- seq(0, 1, by = 0.1)
  
  pp_values <- scenarios$PP
  
  # Use map_dfc() to calculate the parameters for each "PP" value and combine them into a data frame
  parameter_data <- map(pp_values, scenarios)
  
  
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
           "t_kstable" = V13,
           "kUcoeff1" = V14,
           "kUcoeff2" = V15,
           "kWcoeff1" = V16,
           "kWcoeff2" = V17,
           "t_perturb" = V18) %>%
    select(PP, everything())
  
  return(df_parameter_values)
}

generate_parameter_dataframe()






# ==============================================================================
# Define your custom function
my_function <- function(PP, delta) {
  value <- PP * delta * 3
  return(value)
}


calculate_parameters <- function(PP, delta) {
  
  ## Vegetation
  # Deciduous growth rate 
  u_croiss <-  (1+4*PP)*30000 
  
  # # Max deciduous carrying capacity 
  kUpeak  <-  (1 + PP/2)*kUpeak_0
  # 
  # Stable deciduous carrying capacity
  kUstable <- (1+(PP/2))* kUstable_0 * 2.4

  # Shrubs growth rate
  w_0 <-  (1+4*PP)*300      # kg/(km² an), taux de recroissance initial

  # Max shrubs carrying capacity
  kWpeak <- (1+PP/2)*kWpeak_0 # kg/km^2, pour t=t_kpeak

  # Stable shrubs carrying capacity
  kWstable <- (1 + PP/2)*kWstable_0 # kg/km^2, pour t=t_kstable

  # --------------------------------------------------------

  ## Animals
  # Moose carrying capacity
  k_m = ((2-0.84)*PP + 0.84) * delta

  # Devrait être exprimée en fonction de la quantité de feuillus!!!

  # Moose growth rate
  chi_M = m_croiss * ((a_M * e_UM * kUpeak)/
                        (1+a_M * h_UM * kUpeak) - mu_M)^-1

  # Deer carrying capacity
  k_c= ((11.43-4.74)*PP +4.74) * delta

  # Deer growth rate
  chi_C = c_croiss *
    ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1

  # --------------------------------------------------------

  t_perturb = 100            # years
  # #t_pertub = 0

  # Definition des moments suivant la perturbation
  t_low = t_perturb + 5           # years : temps pour atteindre le minimum de
  # biomasse apres une perturbation
  t_kpeak = t_low + (50 - 25*PP) # years :  temps pour atteindre le maximum de
  # biomasse apres une perturbation
  t_kstable = t_kpeak + 100      # years : temps pour atteindre une stabilité de

  kUcoeff1 = (kUstable - kUpeak)/(t_kstable - t_kpeak)
  kUcoeff2 = kUpeak - (kUstable - kUpeak)/(t_kstable - t_kpeak) * t_kpeak
  kWcoeff1 = (kWstable - kWpeak)/(t_kstable - t_kpeak)
  kWcoeff2 = kWpeak - (kWstable - kWpeak)/(t_kstable - t_kpeak) * t_kpeak
  
  return(data.frame(PP, delta, u_croiss, kUpeak, kUstable, w_0, kWpeak, kWstable,
           k_m, k_c, chi_M, chi_C, t_low, t_kpeak,t_kstable,
           kUcoeff1, kUcoeff2, kWcoeff1, kWcoeff2, t_perturb))
  
  # return(data.frame(PP, delta, u_croiss, kUpeak))
}

# Create the scenarios dataset
scenarios <- expand.grid(
  PP = seq(0.9, 1, 0.1),
  delta = seq(1, 1.1, 0.1))

# Apply the custom function to all combinations of "PP" and "delta"
# scenarios$value <- mapply(calculate_parameters, scenarios$PP, scenarios$delta)
# Apply the custom function to all combinations of "PP" and "delta"
results <- do.call(rbind, apply(scenarios, 1, function(row) calculate_parameters(row["PP"], row["delta"])))


# Rename the columns if needed
colnames(results) <- c("PP", "delta", "u_croiss", "kUpeak",
                       "kUstable", "w_0", "kWpeak", "kWstable",
                       "k_m", "k_c", "chi_M", "chi_C", "t_low", "t_kpeak","t_kstable",
                       "kUcoeff1", "kUcoeff2", "kWcoeff1", "kWcoeff2", "t_perturb")





# ==============================================================================
library(purrr)


scenarios <- expand.grid(
  PP = seq(0.9, 1, 0.1),
  delta = seq(1, 1.1, 0.1))


my_function <- function(...) {
  args <- list(...)
  
  PP <- scenarios$PP
  delta <- scenarios$delta
  
  test <- PP + delta
  othertest <- PP * delta
  
  return(data.frame(PP, delta, test, othertest))
  
}


# Apply the custom function to the list of argument lists using pmap
results <- pmap(arguments_list, my_function)

# Bind the results into a single data frame
results_df <- do.call(rbind, results)

# Rename the columns if needed
colnames(results_df) <- c("PP", "delta", "value", "othervalue")




# Your data frame "results_df" contains the results.


