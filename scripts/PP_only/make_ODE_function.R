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


# make_ODE <- function(PP, delta, data){
make_ODE <- function(PP, data){
  
  Unnested_df <-  unnest(data, cols=c())
  # print(Unnested_df)
  
  
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
             U_test = u_init,
             Na_test = na_init,
             Nj_test = nj_init,
             Ma_test = ma_init,
             Mj_test = mj_init,
             # Pa_test = pa_init,
             Ca_test = ca_init,
             Cj_test = cj_init,
             Pa_test = pa_init,
             Pj_test = pj_init)
  # Qa_test = qa_init,
  # Qj_test = qj_init)
  
  # times <- seq(0, 1, by = 1)
  times <- seq(0, 300, by = 0.1)
  # times <- seq(0, 2000, by = 0.1)
  # times <- seq(0, 1, by = 0.1)
  
  # PARS should be one value at a time
  # pars  <- c(
  #   v_croiss = Unnested_df$u_croiss,
  #   k_V = 100,
  #   # tlow = Unnested_df$tlow,
  #   PP = Unnested_df$PP,
  #   parms <- parms,
  #   data <- Unnested_df)
  # 
  
  # delta1 <- data$delta1
  
  Parms <- list(
    
    parms <- parms,
    data <- Unnested_df,
    PP <- PP,
    pouic <- data$pouic,
    chi_M <- data$chi_M)
  # print(delta)
  
  pouic <- data$pouic
  # print(pouic)
  
  # actual_ode <- function(yini, times, parms){
  # print(times)
  # print(PP)
  res   <- ode(y = yini,
               t = times,
               equa_diff_sp,
               parms = Parms,
               method = "rk4") %>% 
    as_tibble()
  
  # Might be switched to "lsoda" or "vode"
  
  # print(length(res))
  # Vector to store new column names for columns 11 to 47
  # new_column_names <- c("new_column_11", "new_column_12", "new_column_13", "new_column_14")
  # Because the first columns take the vector y names
  
  
  # new_column_names <- c("pref_P_Mj",
  #                       "pref_P_Ma",
  #                       "k_P",
  #                       "rfonc_P_Mj",
  #                       "rfonc_P_Ma",
  #                       "rfonc_P_Nj",
  #                "rfonc_P_Na",
  #                "rfonc_tot",
  #                "chi_P",
  #                "surplus_NRJ",
  #                "N_tot",
  #                "M_tot",
  #                "C_tot",
  #                "proies_tot",
  #                "PP",
  #                "p_croiss",
  #                "a_P", 
  #                "mu_P",
  #                "rep_fonc_MU",
  #                "croissance_loup",
  #                "pref_P_Ma_i",
  #                "pref_P_Mj_i",
  #                "pref_P_M",
  #                "den_rfonc_P",
  #                "test",
  #                "test1",
  #                "test3",
  #                "evol_P",
  #                "ma_init",
  #                "na_init",
  #                "ca_init",
  #                "p_init",
  #                "test4",
  #                "k_U",
  #                "k_V",
  #                "rfonc_P_Cj",
  #                "rfonc_P_Ca")
  
  new_column_names <- c("rfonc_P_Mj",
                        "rfonc_P_Ma",
                        "rfonc_P_Nj",
                        "rfonc_P_Na",
                        "rfonc_P_Cj",
                        "rfonc_P_Ca",
                        "rfonc_tot",
                        # "rfonc_P_Qj",
                        # "rfonc_P_Qa",
                        
                        "pref_P_Mj",
                        "pref_P_Ma",
                        "pref_P_Nj",
                        "pref_P_Na",
                        "pref_P_Cj",
                        "pref_P_Ca",
                        # "pref_P_Qj",
                        # "pref_P_Qa",
                        "den_rfonc_P",
                        "k_P",
                        
                        "chi_P",
                        "surplus_NRJ",
                        "M_tot",
                        "N_tot",
                        "C_tot",
                        "proies_tot",
                        "PP",
                        "p_croiss",
                        "a_P",
                        "mu_P",
                        "rep_fonc_MU",
                        "croissance_loup",
                        # "den_rfonc_P",
                        # "test",
                        # "test1",
                        # "test3",
                        "evol_P",
                        "ma_init",
                        "na_init",
                        "ca_init",
                        "p_init",
                        # "test4",
                        "k_U",
                        "k_V",
                        "rep_totale_MU",
                        "rep_fonc_CU",
                        "rep_totale_CU",
                        "delta(pouic)",
                        "k_m",
                        "Pa",
                        "Ma",
                        "Mj",
                        "U",
                        "juv_growth", "transition_adu",
                        "limite_enviro1","limite_enviro2",
                        "predation",
                        "k_c",
                        "Ca",
                        "Cj",
                        "conv_adu_C",
                        "juv_growth_C",
                        "predation_C",
                        "k_n",
                        "Na",
                        "Nj",
                        "rfonc_tot_test",
                        "rfonc_P_Na_test",
                        "rfonc_P_Nj_test",
                        "surplus_NRJ_test",
                        "rfonc_P_Ma_test",
                        "rfonc_P_Mj_test",
                        "rfonc_P_Ma_Messier",
                        "rfonc_P_Mj_Messier",
                        "rep_fonc_Messier",
                        "rep_fonc_turchin",
                        "surplus_NRJ_Messier",
                        "k_P_Messier",
                        "wolf_decrease",
                        "Pj",
                        "growth_young_wolf",
                        "Ma_supplementary_NRJ",
                        "chi_M",
                        "Ma_supplementary_NRJ_test",
                        "Na_supplementary_NRJ",
                        "Ca_supplementary_NRJ",
                        "k_P_Johnson",
                        "ungulate_biomass"
                        # "Qa",
                        # "Qj"
  )
  
  
  # 
  # # 
  # Get the names of columns to rename (11 to 47)
  columns_to_rename <- paste0("V", 12:90)
  
  # Rename specific columns using rename_at()
  res <- res %>%
    rename_at(vars(all_of(columns_to_rename)), ~ new_column_names) %>% 
    mutate(Pa_test = Pa,
           Pj_test = Pj,
           Ma_test= Ma,
           Mj_test = Mj,
           U_test = U,
           Ca_test = Ca,
           Cj_test = Cj,
           Na_test = Na,
           Nj_test = Nj
           # Qa_test = Qa,
           # Qj_test = Qj
           ) %>% 
    select(-c(Pa,
              Pj,
              Ma,
              Mj,
              U,
              Ca,
              Cj,
              Na,
              Nj
              # Qa,
              # Qj
              )) %>% 
    rename(Pa = Pa_test,
           Pj = Pj_test,
           Ma = Ma_test,
           Mj = Mj_test,
           U = U_test,
           Ca = Ca_test,
           Cj = Cj_test,
           Na = Na_test,
           Nj = Nj_test
           # Qa = Qa_test,
           # Qj = Qj_test
           )
  
  
  # return(res)
  # }
  # res <- actual_ode(yini, times, parms)
  return(res)
  
}
