# equa_diff_sp_test_nest <- function(t, y, parms){
#   V  <- y[1]
#   with(as.list(c(y, parms)), {
# 
#     int_rlt <- intermediate_res(y)
#     r_fonc <- int_rlt[[1]]
# 
#     rfonc_P_Mj <- r_fonc[1]
# 
#     dVdt <- v_croiss * V * (1 - V/k_V)
#     return(list(c(dVdt)))
#   })
# }


equa_diff_sp <- function(t,y, Parms){
  # On decrit dans le vecteur `initial_values` la place
  # de chaque variable. Il faut toujours garder le même ordre.
  # En gros, on dit que pour trouver la valeur de V au temps 
  # t, il faut regarder dans la première colonne du 
  # vecteur y. etc
  
  # PP <- 

  # Parms <- c(parms, data, PP)
  
  # print(Parms)
  
  parms <- Parms[[1]]
  data <- Parms[[2]]
  PP <- Parms[[3]]
  pouic <- Parms[[4]]
  
  # print(delta1)
  
  V  <- y[1]
  U  <- y[2]
  Na <- y[3]
  Nj <- y[4]
  Ma <- y[5]
  Mj <- y[6]
  P  <- y[7]
  Ca <- y[8]
  Cj <- y[9]
  
  
  
  with(as.list(parms), {
    
    
    # if (Mj < 0){
    #   Mj = 0}
    # 
    # if (Ma < 0){
    #   Ma = 0}
    # 
    # # PROVISOIRE
    # if (any(is.na(Nj))) {
    #   Nj[is.na(Nj)] <- 0
    # }
    # 
    # # if (Nj < 0) {
    # #   Nj <- 0
    # # }
    # 
    # if (Na < 0){
    #   Na = 0}
    # 
    # if (Cj < 0){
    #   Cj = 0}
    # 
    # if (Ca < 0){
    #   Ca = 0}
    
    # Reponses fonctionnelle predateur-proie
    
    
    int_rlt <- intermediate_res(y)
    r_fonc <- int_rlt[[1]]
    
    rfonc_P_Mj <- r_fonc[1]
    rfonc_P_Ma <- r_fonc[2]
    rfonc_P_Nj <- r_fonc[3]
    rfonc_P_Na <- r_fonc[4]
    rfonc_P_Cj <- r_fonc[5]
    rfonc_P_Ca <- r_fonc[6]
    rfonc_tot <- r_fonc[7] 
    
    par_pref <- int_rlt[[2]]
    pref_P_Mj <- par_pref[1]
    pref_P_Ma <- par_pref[2]
    pref_P_Nj <- par_pref[3]
    pref_P_Na <- par_pref[4]
    pref_P_Cj <- par_pref[5]
    pref_P_Ca <- par_pref[6]
    
    den_rfonc_P <- int_rlt[[3]]
    k_P <- int_rlt[[4]]
    M_tot <- int_rlt [[5]]
    N_tot <- int_rlt [[6]]
    C_tot <- int_rlt[[7]]
    proies_tot <- int_rlt[[8]]
    chi_P <- int_rlt[[9]]
    surplus_NRJ <- int_rlt[[10]]
    rep_fonc_MU <- int_rlt[[11]]
    croissance_loup <- int_rlt[[12]]
    mu_P <- int_rlt[[13]]
    test <- int_rlt[[14]]
    test1 <- int_rlt[[15]]
    test3 <- int_rlt[[16]]
    evol_P <- int_rlt[[17]]
    test4 <- int_rlt[[18]]
    rep_totale_MU <- int_rlt[[19]]
    rep_fonc_CU <- int_rlt[[20]]
    rep_totale_CU <- int_rlt[[21]]
    
    # Evolution des capacites de charge de la vgtation
    # en fct des perturbations
    # print(PP)
    capacite_vg <- evol_vg(t,data, PP)
    k_U <- capacite_vg[[1]]
    k_W <- capacite_vg[[2]]
    k_V <- capacite_vg[[3]]
    # k_V <- capacite_vg[[2]] # A MODIFIER !!!!!!!!!!!
    
    
    u_croiss = data$u_croiss
    w_0 <- data$w_0
    kWpeak <- data$kWpeak
    kWstable <- data$kWstable
    k_m <- data$k_m
    k_c <- data$k_c
    chi_M <- data$chi_M
    chi_C <- data$chi_C
    kUcoeff1 <- data$kUcoeff1 
    kUcoeff2 <- data$kUcoeff2
    kWcoeff1 <- data$kWcoeff1 
    kWcoeff2 <- data$kWcoeff2
    
    
    # # Trigger effect
    # extr_event <- trigger_fct(t,y)
    # verif <- extr_event
    
    # Lichen
    # Modèle logistique
    dVdt <- v_croiss * V * (1 - V/k_V) -
      ((a_N * V * (Na+Nj))/(1 + a_N * h_VN * V))
    
    # Feuillus
    # Modèle exponentiel
    dUdt <- u_croiss * (1 - U/k_U) -
      ((a_M * U * (Ma+Mj))/(1 + a_M * h_UM * U)) -
      ((a_C * U * (Ca+Cj))/(1 + a_C * h_UC * U))
    
    
    # Caribou adulte
    dNadt <- tau_n * Nj - 
      ((n_croiss/k_n) * Na * (Na+Nj)) - rfonc_P_Na * P
    
    # Caribou jeune
    dNjdt <- chi_N * ((a_N * e_VN * V)/(1 + a_N * h_VN * V) - mu_N) * Na -
      tau_n * Nj - 
      ((n_croiss/k_n) * Nj * (Na+Nj)) -
      rfonc_P_Nj * P
    
    # Orignal adulte
    dMadt <- tau_m * Mj -
      ((m_croiss/k_m) * Ma * (Ma+Mj)) - rfonc_P_Ma * P
    
    # Orignal juvenile
    dMjdt <- chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma -
      tau_m * Mj - 
      ((m_croiss/k_m) * Mj * (Ma+Mj)) -
      rfonc_P_Mj * P
    
    # Cerfs adultes
    dCadt <- tau_c * Cj -
      ((c_croiss/k_c) * Ca * (Ca+Cj)) - rfonc_P_Ca * P
    
    # Cerf juveniles
    dCjdt <- chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca -
      tau_c * Cj - 
      ((c_croiss/k_c) * Cj * (Ca+Cj)) -
      rfonc_P_Cj * P
    
    
    # Loup
    # dPdt <- chi_P * 
    #   ((rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
    #                    epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj))- mu_P) * P - (p_croiss/k_P) * P^2
    # 
    dPdt <- chi_P * surplus_NRJ * P - ((p_croiss/k_P) * P^2)
    
    
    return (list(c(dVdt, dUdt, dNadt, dNjdt, dMadt, dMjdt,dPdt,
                   dCadt, dCjdt),
                 
                 rfonc_P_Mj,
                 rfonc_P_Ma,
                 rfonc_P_Nj,
                 rfonc_P_Na,
                 rfonc_P_Cj,
                 rfonc_P_Ca,
                 rfonc_tot,
                 
                
                 pref_P_Mj,
                 pref_P_Ma,
                 pref_P_Nj,
                 pref_P_Na,
                 pref_P_Cj,
                 pref_P_Ca,
                 den_rfonc_P,
                 k_P,
                 
                 chi_P,
                 surplus_NRJ,
                 M_tot,
                 N_tot,
                 C_tot,
                 proies_tot,
                 PP,
                 p_croiss,
                 a_P, 
                 mu_P,
                 rep_fonc_MU,
                 croissance_loup,
                 # den_rfonc_P,
                 # test,
                 # test1,
                 # test3,
                 evol_P,
                 ma_init,
                 na_init,
                 ca_init,
                 p_init,
                 # test4,
                 k_U,
                 k_V,
                 rep_totale_MU,
                 rep_fonc_CU,
                 rep_totale_CU,
                 pouic))
  })
}
