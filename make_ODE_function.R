# make_ODE <- function(PP, data){
#   
#   Unnested_df <-  unnest(data, cols=c())
#   
#   yini  <- c(V = 1)
#   times <- seq(0, 200, by = 1)
#   
#   # PARS should be one value at a time
#   parms  <- c(
#     v_croiss = Unnested_df$u_croiss,
#     k_V = 100)
#   
#   actual_ode <- function(yini, times, parms){
#     out   <- ode(y = yini,
#                  times,
#                  equa_diff_sp_test_nest,
#                  parms,
#                  method = "rk4") %>% 
#       as_tibble()
#     return(out)
#   }
#   res <- actual_ode(yini, times, parms)
#   return(res)
#   
# }


make_ODE <- function(PP, data){
  
  Unnested_df <-  unnest(data, cols=c())
  
  # yini  <- c(V = 1,
  #            U = 1,
  #            Na = 1,
  #            Nj = 1,
  #            Ma = 1,
  #            Mj = 1,
  #            P = 1,
  #            Ca = 1,
  #            Cj = 1)
  
  # Takes the values coming from the static_fauna_parameters
  # script
  yini  <- c(V = v_init,
             U = u_init,
             Na = na_init,
             Nj = nj_init,
             Ma = ma_init,
             Mj = mj_init,
             P = p_init,
             Ca = ca_init,
             Cj = cj_init)
  
  times <- seq(0, 200, by = 1)
  
  # PARS should be one value at a time
  # pars  <- c(
  #   v_croiss = Unnested_df$u_croiss,
  #   k_V = 100,
  #   # tlow = Unnested_df$tlow,
  #   PP = Unnested_df$PP,
  #   parms <- parms,
  #   data <- Unnested_df)
  # 
  
  Parms <- list(
    
    parms <- parms,
    data <- Unnested_df,
    PP <- PP)
  
  
  # actual_ode <- function(yini, times, parms){
  # print(times)
  # print(PP)
  res   <- ode(y = yini,
               t = times,
                 equa_diff_sp,
                 parms = Parms,
                 method = "rk4") %>% 
    as_tibble()
  
  
  # Vector to store new column names for columns 11 to 47
  # new_column_names <- c("new_column_11", "new_column_12", "new_column_13", "new_column_14")
  
  new_column_names <- c("pref_P_Mj", "pref_P_Ma", "k_P", "rfonc_P_Mj",  "rfonc_P_Ma",
                 "rfonc_P_Nj",
                 "rfonc_P_Na",
                 "rfonc_tot",
                 "chi_P",
                 "surplus_NRJ",
                 "N_tot",
                 "M_tot",
                 "C_tot",
                 "proies_tot",
                 "PP",
                 "p_croiss",
                 "a_P", 
                 "mu_P",
                 "rep_fonc_MU",
                 "croissance_loup",
                 "pref_P_Ma_i",
                 "pref_P_Mj_i",
                 "pref_P_M",
                 "den_rfonc_P",
                 "test",
                 "test1",
                 "test3",
                 "evol_P",
                 "ma_init",
                 "na_init",
                 "ca_init",
                 "p_init",
                 "test4",
                 "k_U",
                 "k_V")
  
  # Get the names of columns to rename (11 to 47)
  columns_to_rename <- paste0("V", 11:45)
  
  # Rename specific columns using rename_at()
  res <- res %>%
    rename_at(vars(all_of(columns_to_rename)), ~ new_column_names)

  
    # return(res)
  # }
  # res <- actual_ode(yini, times, parms)
  return(res)
  
}
