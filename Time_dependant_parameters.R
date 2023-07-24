# # -------------------------------------------------------
# 
# # Modelisation de l'evolution de la vegetation en fct 
# # des valeurs initiales et d'une perturbation
# 
# # Calcul les evolutions des capacites de charge en fct 
# # du temps

evol_vg <- function(t,
                    t_low,
                    t_kpeak,
                    t_kstable,
                    kUstable,
                    kUcoeff1,
                    kUcoeff2,
                    kUpeak){

# evol_vg <- function(t){
#   
#   
#   t_low <-  test[i,"t_low"]
#   t_kpeak <-  test[i,"t_kpeak"]
#   t_kstable <- test[i,"t_kstable"]
#   kUstable <- test[i,"kUstable"]
#   kUcoeff1 <- test[i,"kUcoeff1"]
#   kUcoeff2 <- test[i,"kUcoeff2"]
#   kUpeak <- test[i,"kUpeak"]
  # t_low_long <- data$t_low
  # 
  # for (i in 1:length(data)){
  #   
  #   parms["t_low"] <- t_low_long[i]
  #   
  # return(c(parms["t_low"]))
  # 
  # }}

# evol_vg(test)


  # t_low_long <- test$t_low
  # t_pertub <- 100
  # t_kpeak <- test$t_kpeak
  # t_kstable <-test$t_kstable
  # kUstable <-  test$kUstable
  # kUcoeff1 <- test$kUcoeff1
  # kUcoeff2 <- test$kUcoeff2
  # kUpeak <- test$kUpeak
# 
#   t_low <-  100
#   t_pertub <- 100
#   t_kpeak <- 100
#   t_kstable <- 100
#   kUstable <- 100
#   kUcoeff1 <- 100
#   kUcoeff2 <- 100
#   kUpeak <- 100
  
    if (t_perturb <= t & t < t_low) {
    # k_U = 0
    # k_W = 0
    # k_V = 0
    k_U = kUlow
    # k_W = kWlow
    k_V = kVlow
  } else if (t_low <= t & t < t_kpeak) {
    # k_U = 1
    # k_W = 1
    # k_V = 1
    k_U = kUpeak
    # k_W = kWpeak
    k_V = kVnorm
  } else if (t_kpeak <= t & t <= t_kstable) {
    # k_U = 1.1
    # k_W = 1.1
    # k_V = 1.1
    k_U = kUcoeff1*t + kUcoeff2
    # k_W = kWcoeff1*t + kWcoeff2
    k_V = kVnorm
  } else {
    # k_U = 1.3
    # k_W = 1.3
    # k_V = 1.3
    k_U = kUstable
    # k_W = kWstable
    k_V = kVnorm
  }
  # return(list(c(k_U,k_W,k_V)))
  # res <- c(k_U,k_W,k_V)
  res <- c(k_U, k_V)
  return(c(k_U,k_V))
  }
# 
# # b <- evol_vg(1,y0)
# # a <- evol_vg(100,y0)


# Make it vary in time
rte <- function(t_low,
                t_kpeak,
                t_kstable,
                kUstable,
                kUcoeff1,
                kUcoeff2,
                kUpeak) {
  
  # Create a vector with "PP" values from 0 to 10 with incrementation of 0.1
  times <- seq(0, 200, by = 1)
  
  # parms <- list(times,
  #               t_low,
  #               t_kpeak,
  #               t_kstable,
  #               kUstable,
  #               kUcoeff1,
  #               kUcoeff2,
  #               kUpeak)
  # parms <- as.numeric(c(t_low = test$t_low))
  
  # Use map_dfc() to calculate the parameters for each "PP" value and combine them into a data frame
  jfregio <- map(times, ~evol_vg(.x,
                                 t_low,
                                 t_kpeak,
                                 t_kstable,
                                 kUstable,
                                 kUcoeff1,
                                 kUcoeff2,
                                 kUpeak))
  # jfregio <- pmap(parms,  ~evol_vg())
  
  # map2tre <- pmap(times, result_df, ~evol_vg(.x,.y))
  
  # Add the "PP" column to the data frame
  # df_parameter_values <- bind_cols(PP = pp_values, df_parameter_values)
  rety <- as.data.frame(do.call(rbind, jfregio))
  
  
  # Add the time column
  rety$time <- times
  
  # Rename the columns and reorder with "time" column first
  rety <- rety %>% 
    rename("k_U" = V1,
           "k_V" = V2) %>%
    select(time, everything())
  
  return(rety)
}
# 
# 

# FONCTIONNE SI L'ON DEFINIT UNE A UNE LES VALEURS
# SUIVANTES

# rte()
# 
# t_perturb <- 50
# t_low <- 100
# t_kpeak <- 100
# t_kstable <-100
# kUstable <-  165
# kUcoeff1 <- 4567
# kUcoeff2 <- 4567


# Objectif: prendre les valeurs qui sont inscrites dans 
# le df sortant de auto_gen_PP related parameters 



evol_vg_light <- function(t, data){
  
  
  if (t_perturb <= t & t < t_kpeak) {
    k_U = 0
    # k_W = 0
    k_V = 0
    # k_U = kUlow
    # k_W = kWlow
    # k_V = kVlow
  } else {
    k_U = 1.3
    # k_W = 1.3
    k_V = 1.3
    # k_U = kUstable
    # k_W = kWstable
    # k_V = kVnorm
  }
  # return(list(c(k_U,k_W,k_V)))
  # res <- c(k_U,k_W,k_V)
  res <- c(k_U, k_V)
}
# 
# # b <- evol_vg(1,y0)
# # a <- evol_vg(100,y0)
# 
# 
DF1



# Make it vary in time
rte_light <- function() {
  
  # Create a vector with "PP" values from 0 to 10 with incrementation of 0.1
  times <- seq(0, 200, by = 1)
  
  # Use map_dfc() to calculate the parameters for each "PP" value and combine them into a data frame
  jfregio <- map(times, ~evol_vg_light(.x))
  
  # map2tre <- pmap(times, result_df, ~evol_vg(.x,.y))
  
  # Add the "PP" column to the data frame
  # df_parameter_values <- bind_cols(PP = pp_values, df_parameter_values)
  rety <- as.data.frame(do.call(rbind, jfregio))
  
  
  # Add the time column
  rety$time <- times
  
  # Rename the columns and reorder with "time" column first
  rety <- rety %>% 
    rename("k_U" = V1,
           "k_V" = V2) %>%
    select(time, everything())
  
  return(rety)
}
# 