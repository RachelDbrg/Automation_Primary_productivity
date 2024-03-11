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
  
  if (U <= 0){
    U = 0}
  else{
    U  <- U
  }
  
  # print(paste0("U_sp_eq", U))
  
  Na <- y[3]
  
  if (Na <= 0){
    Na = 0}
  else{
    Na  <- Na
  }
  
  # print(paste0("Na=", Na))
  
  Nj <- y[4]
  
  if (Nj <= 0){
    Nj = 0}
  else{
    Nj  <- Nj
  }
  
  # print(paste0("Nj=", Nj))
  
  
  Ma <- y[5]
  
  # print(Ma)
  
  if (Ma <= 0){
    Ma = 0}
  else{
    Ma  <- Ma
  }
  
  # print(paste0("Ma_sp_eq", Ma))
  
  # print(paste0("Ma_post_transfor=", Ma))
  
  Mj <- y[6]
  # 
  if (Mj <= 0){
    Mj = 0}
  else{
    Mj  <- Mj
  }
  
  
  # P  <- y[7]
  # 
  # # print(paste0("y[7]=", y[7]))
  # 
  # 
  # 
  # if (P <= 0){
  #   P = 0}
  # else{
  #   P  <- P
  # }
  
  Pa  <- y[9]
  
  # print(paste0("y[7]=", y[7]))
  
  
  
  if (Pa <= 0){
    Pa = 0}
  else{
    Pa  <- Pa
  }
  
  
  # print(paste0("Pa=", Pa))
  
  Pj  <- y[10]
  
  # print(paste0("y[10]=", y[10]))
  
  
  
  if (Pj <= 0){
    Pj = 0}
  else{
    Pj  <- Pj
  }
  
  # print(paste0("Pj=", Pj))
  
  Ca <- y[7]
  
  if (Ca <= 0){
    Ca = 0}
  else{
    Ca  <- Ca
  }
  
  
  Cj <- y[8]
  
  if (Cj <= 0){
    Cj = 0}
  else{
    Cj  <- Cj
  }
  # 
  # Qa <- y[11]
  # # 
  # if (Qa <= 0){
  #   Qa = 0}
  # else{
  #   Qa  <- Qa
  # }
  # 
  # # 
  # Qj <- y[12]
  # 
  # if (Qj <= 0){
  #   Qj = 0}
  # else{
  #   Qj  <- Qj
  # }
  # 
  
  
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
    # rfonc_P_Qj <- r_fonc[8]
    # rfonc_P_Qa <- r_fonc[9]
    
    par_pref <- int_rlt[[2]]
    pref_P_Mj <- par_pref[1]
    pref_P_Ma <- par_pref[2]
    pref_P_Nj <- par_pref[3]
    pref_P_Na <- par_pref[4]
    pref_P_Cj <- par_pref[5]
    pref_P_Ca <- par_pref[6]
    # pref_P_Qj <- par_pref[7]
    # pref_P_Qa <- par_pref[8]
    
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
    rfonc_tot_test <- int_rlt[[24]]
    rfonc_P_Na_test <- int_rlt[[25]]
    rfonc_P_Nj_test <- int_rlt[[26]]
    surplus_NRJ_test <- int_rlt[[27]]
    rfonc_P_Ma_test <- int_rlt[[28]]
    rfonc_P_Mj_test <- int_rlt[[29]]
    rfonc_P_Ma_Messier <- int_rlt[[30]]
    rfonc_P_Mj_Messier <- int_rlt[[31]]
    rep_fonc_Messier <- int_rlt[[32]]
    surplus_NRJ_Messier <- int_rlt[[33]]
    k_P_Messier <- int_rlt[[34]]
    wolf_decrease <- int_rlt[[35]]
    # chi_M <- int_rlt[[36]]
    Ma_supplementary_NRJ <- int_rlt[[36]]
    Ma_supplementary_NRJ_test <- int_rlt[[37]]
    Na_supplementary_NRJ <- int_rlt[[38]]
    Ca_supplementary_NRJ <- int_rlt[[39]]
    k_P_Johnson <- int_rlt[[40]]
    ungulate_biomass <- int_rlt[[41]]
    # Qa_supplementary_NRJ <- int_rlt[[42]]
    # print(paste0("surplus_NRJ_test=", surplus_NRJ_test))
    
    # print(paste0("surplus_NRJ=", surplus_NRJ))
    
    
    # Evolution des capacites de charge de la vgtation
    # en fct des perturbations
    # print(PP)
    capacite_vg <- evol_vg(t,data, PP)
    k_U <- capacite_vg[[1]]
    k_W <- capacite_vg[[2]]
    k_V <- capacite_vg[[3]]
    # k_V <- capacite_vg[[2]] # A MODIFIER !!!!!!!!!!!
    
    
    # print(paste("kU=", k_U))
    
    u_croiss = data$u_croiss
    w_0 <- data$w_0
    kWpeak <- data$kWpeak
    kWstable <- data$kWstable
    # k_m <- data$k_m
    k_m <- int_rlt[[22]]
    # k_m <- 5
    
    # print(paste("k_m_sp_eq", k_m))
    # k_c <- data$k_c
    k_c <- int_rlt[[23]]
    k_n <- int_rlt[[42]]
    
    # print(paste("k_c_sp_eq", k_c))
    
    chi_M <- data$chi_M
    chi_C <- data$chi_C
    kUcoeff1 <- data$kUcoeff1 
    kUcoeff2 <- data$kUcoeff2
    kWcoeff1 <- data$kWcoeff1 
    kWcoeff2 <- data$kWcoeff2
    
    chi_N <- data$chi_N
    
    
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
    
    # print(paste("u_croiss * (1 - U/k_U)=", u_croiss * (1 - U/k_U)))
    # print(paste("(1 - U/k_U)=", (1 - U/k_U)))
    # print(paste("kU=", k_U))
    # print(paste("((a_M * U * (Ma+Mj))/(1 + a_M * h_UM * U))=", ((a_M * U * (Ma+Mj))/(1 + a_M * h_UM * U))))
    # print(paste("((a_C * U * (Ca+Cj))/(1 + a_C * h_UC * U))=", ((a_C * U * (Ca+Cj))/(1 + a_C * h_UC * U))))
    # print(paste("dUdt=", dUdt))
    
    
    # Caribou adulte
    # dNadt <- tau_n * Nj -
    #   ((n_croiss/k_n) * Na * (Na+Nj)) - rfonc_P_Na * P
    
    # dNadt <- tau_n * Nj* (1-((Na)/k_n)) - rfonc_P_Na * P
    
    
    # VRAIE EQUATION
    # dNadt <- tau_n * Nj* (1-((Na)/k_n)) - rfonc_P_Na_test * P
    
    # EQUATION TEST
    # dNadt <- tau_m * Nj * (1-((Na)/k_m)) - rfonc_P_Na * Pa
    dNadt <- tau_n * Nj * (1-((Na)/k_n)) - rfonc_P_Na * Pa
    
    # dNadt <- tau_n * Nj -
    #   ((n_croiss/k_n) * Na * (Na)) - rfonc_P_Na * P
    
    # Caribou jeune
    # dNjdt <- chi_N * ((a_N * e_VN * V)/(1 + a_N * h_VN * V) - mu_N) * Na -
    #   tau_n * Nj - 
    #   # ((n_croiss/k_n) * Nj * (Na+Nj)) -
    #   rfonc_P_Nj * P
    
    # VRAIE EQUATION
    # dNjdt <- chi_N * ((a_N * e_VN * V)/(1 + a_N * h_VN * V) - mu_N) * Na -
    #   tau_n * Nj - 
    #   # ((n_croiss/k_n) * Nj * (Na+Nj)) -
    #   rfonc_P_Nj_test * P
    
    # EQUATION TEST
    # dNjdt <- chi_M * Ma_supplementary_NRJ * Na -
    #   tau_m * Nj -
    #   # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   # ((m_croiss/k_m) * Ma * (Ma)) -
    #   rfonc_P_Nj * Pa
    
    dNjdt <- chi_N * Na_supplementary_NRJ * Na -
      tau_n * Nj -
      # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
      # ((m_croiss/k_m) * Ma * (Ma)) -
      rfonc_P_Nj * Pa
    
    
    
    
    # # Orignal adulte
    # dMadt <- tau_m * Mj -
    #   ((m_croiss/k_m) * Ma * (Ma+Mj)) - rfonc_P_Ma * P
    
    # dMadt <- tau_m * Mj -
    #   ((m_croiss/k_m) * Ma * (Ma)) - rfonc_P_Ma * P
    
    # dMadt <- tau_m * Mj
    
    # dMadt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma * P
    
    dMadt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma * Pa
    
    # Following Turchin2003
    # dMadt <- 0.2 * Ma * (1 - (Ma/k_m)) - (12.3*P*Ma/0.47+Ma)
    # dMadt <- 0.2 * Ma * (1 - (Ma/2)) - (12.3*P*Ma/0.47+Ma)
    
    # dMadt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma_test * P
    
    rep_fonc_turchin <- (12.3*Pa*Ma/0.47+Ma)
    
    
    limit_enviro1 <- ((m_croiss/k_m) * Mj * (Ma+Mj)) 
    limit_enviro2 <- Ma/k_m 
    
    # print(paste("Ma=", Ma))
    # print(paste("Mj=", Mj))
    # print(paste("dMadt_sp_eq=", dMadt))
    # print(paste("Ma_eq=", Ma))
    
    # print(paste("tau_m * Mj=", tau_m * Mj))
    # print(paste("(m_croiss/k_m) * Ma * (Ma+Mj)=", (m_croiss/k_m) * Ma * (Ma+Mj)))
    # print(paste("m_croiss=", m_croiss))
    # print(paste("k_m_sp_eq=", k_m))
    # print(paste("(m_croiss/k_m)=", (m_croiss/k_m)))
    
    # print(paste("Mj_sp_eq=", Mj))
    # print(paste("Ma_sp_eq=", Ma))
    # print(paste("rfonc_P_Ma_sp_eq=", rfonc_P_Ma))
    # print(paste("P_sp_eq=", P))
    # 
    
    
    # Orignal juvenile
    # dMjdt <- chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma -
    #   tau_m * Mj - 
    #   ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   rfonc_P_Mj * P
    
    # Ma_supplementary_NRJ <- (((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M))
    
    
    dMjdt <- chi_M * Ma_supplementary_NRJ * Ma -
      tau_m * Mj -
      # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
      # ((m_croiss/k_m) * Ma * (Ma)) -
      rfonc_P_Mj * Pa
    
    # dMjdt <- m_croiss * Ma_supplementary_NRJ * Ma -
    #   tau_m * Mj -
    #   # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   # ((m_croiss/k_m) * Ma * (Ma)) -
    #   rfonc_P_Mj * P
    
    # dMjdt <- chi_M * Ma_supplementary_NRJ * Ma -
    #   tau_m * Mj - 
    #   # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   # ((m_croiss/k_m) * Ma * (Ma)) -
    #   rfonc_P_Ma_test * P
    
    
    # print(paste("chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma=",
    # chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma))
    
    # print(paste("chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)=",
    # chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)))
    
    # print(paste("chi_M= ", chi_M))
    
    juv_growth <- chi_M * Ma_supplementary_NRJ * Ma 
    
    # print(paste("chi_M= ", chi_M))
    # print(paste("(a_M * e_UM * U)= ", (a_M * e_UM * U)))
    # print(paste("(1 + a_M * h_UM * U)= ", (1 + a_M * h_UM * U)))
    # print(paste("((a_M * e_UM * U)/(1 + a_M * h_UM * U)= ", ((a_M * e_UM * U)/(1 + a_M * h_UM * U))))
    # print(paste("((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)= ", ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)))
    
    
    conv_adu <- tau_m * Mj 
    
    # limit_enviro <- ((m_croiss/k_m) * Mj * (Ma+Mj)) 
    
    predation <- rfonc_P_Mj * Pa
    
    # Cerfs adultes
    # dCadt <- tau_c * Cj -
    #   ((c_croiss/k_c) * Ca * (Ca+Cj)) - rfonc_P_Ca * P
    
    # dCadt <- tau_c * Cj -
    # ((c_croiss/k_c) * Ca * (Ca)) - rfonc_P_Ca * P # donne un peu plus
    # que la valeur de capacite de cahrge
    
    # dCadt <- tau_c * Cj -
    #   ((c_croiss/k_c) * Ca * (Ca)) - rfonc_P_Ca * P
    
    dCadt <- tau_c * Cj * (1-((Ca)/k_c)) - rfonc_P_Ca * Pa
    
    
    # conv_adu_C <- tau_c * Cj 
    
    # Ca_growth <-  tau_c * Cj
    juv_growth_C <- chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca 
    conv_adu_C <- tau_c * Cj
    predation_C <- rfonc_P_Cj * Pa
    
    
    
    # Cerf juveniles
    # dCjdt <- chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca -
    #   tau_c * Cj - 
    #   ((c_croiss/k_c) * Cj * (Ca+Cj)) -
    #   rfonc_P_Cj * P
    
    dCjdt <- chi_C * Ca_supplementary_NRJ * Ca -
      tau_c * Cj - 
      # ((c_croiss/k_c) * Cj * (Ca+Cj)) -
      rfonc_P_Cj * Pa
    
    # 
    # # Factice additional prey Q
    # dQadt <- tau_c * Qj * (1-((Qa)/k_c)) - rfonc_P_Qa * Pa
    # # 
    # dQjdt <- chi_C * Qa_supplementary_NRJ * Qa -
    #   tau_c * Qj -
    #   # ((c_croiss/k_c) * Cj * (Ca+Cj)) -
    #   rfonc_P_Qj * Pa
    
    # print(paste("Ca=", Ca))
    # print(paste("Cj=", Cj))
    # print(paste("dCadt_sp_eq=", dCadt))
    # print(paste("tau_c * Cj=", tau_c * Cj))
    # print(paste("tau_c * Cj - (1-((Ca)/k_c))=", tau_c * Cj - (1-((Ca)/k_c))))
    # print(paste("c_croiss=", c_croiss))
    # print(paste("k_c_sp_eq=", k_c))
    
    # print(paste("Mj_sp_eq=", Mj))
    # print(paste("Ma_sp_eq=", Ma))
    # print(paste("rfonc_P_Ca_sp_eq=", rfonc_P_Ca))
    # print(paste("predation Ca=", rfonc_P_Ca * P ))
    # print(paste("P_sp_eq=", P))
    
    # print(paste("chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U)", chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U))))
    
    # print(paste("chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca", chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca))
    
    
    # Loup
    # dPdt <- chi_P * 
    #   ((rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
    #                    epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj))- mu_P) * P - (p_croiss/k_P) * P^2
    # 
    # dPdt <- chi_P * surplus_NRJ * P - ((p_croiss/k_P) * P^2)
    
    # dPdt_test <- chi_P * surplus_NRJ_test * P - ((p_croiss/k_P) * P^2)
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P - ((p_croiss/k_P) * P^2)
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P - ((p_croiss/k_P) * P^2) - 0.5*P
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P - ((p_croiss/k_P) * P^2) - 0.5*P
    
    # dPadt <- chi_P * surplus_NRJ_Messier * P * (1 - (P/k_P)) - 0.5*P
    
    dPadt <- tau_p * Pj * (1 - (Pa/k_P)) - 0.2*Pa
    
    # dPjdt <- chi_P * surplus_NRJ_Messier * Pa * (1 - (Pa/k_P)) - tau_p * Pj #- 0.4*Pj #(pup death)
    
    dPjdt <- chi_P * surplus_NRJ * Pa * (1 - (Pa/(k_P+2.2204e-16))) - tau_p * Pj #- 0.4*Pj #(pup death)
    
    # print(paste0("chi_P", chi_P))
    # print(paste0("surplus_NRJ", surplus_NRJ))
    # print(paste0("k_P", k_P))
    # print(paste0("tau_p", tau_p))
    # print(paste0("chi_P * surplus_NRJ * Pa * (1 - (Pa/k_P))", chi_P * surplus_NRJ * Pa * (1 - (Pa/k_P))))
    
    growth_young_wolf <- chi_P * surplus_NRJ_Messier * Pa
    
    # dPdt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma * P
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P * (1) -((P/k_P))
    
    # dPdt <- P * 
    
    # Following Turchin2003
    # dPdt <- 0.1 *((12*Ma/0.47+Ma)-6)*P - ((p_croiss/k_P) * P^2)
    
    
    # print(paste0("dPdt=", dPdt))
    # print(paste0("chi_P", chi_P))
    # print(paste0("surplus_NRJ", surplus_NRJ))
    # print(paste0("dens_P_2=", P))
    # print(paste0("chi_P * surplus_NRJ * P", chi_P * surplus_NRJ * P))
    # print(paste0("((p_croiss", ((p_croiss))))
    # print(paste0("((p_croiss/k_P) * P^2)", ((p_croiss/k_P) * P^2)))
    # print(paste0("k_P", k_P))
    
    # P_correct <- P
    
    k_M_theorique = 0.000002*U - 0.679
    
    return (list(c(dVdt, dUdt, dNadt, dNjdt, dMadt, dMjdt,
                   dCadt, dCjdt,  dPadt, dPjdt),
                 
                 rfonc_P_Mj,
                 rfonc_P_Ma,
                 rfonc_P_Nj,
                 rfonc_P_Na,
                 rfonc_P_Cj,
                 rfonc_P_Ca,
                 rfonc_tot,
                 # rfonc_P_Qj,
                 # rfonc_P_Qa,
                 
                 pref_P_Mj,
                 pref_P_Ma,
                 pref_P_Nj,
                 pref_P_Na,
                 pref_P_Cj,
                 pref_P_Ca,
                 # pref_P_Qj,
                 # pref_P_Qa,
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
                 pouic,
                 k_m,
                 Pa,
                 Ma,
                 Mj,
                 U,
                 juv_growth,
                 conv_adu,
                 limit_enviro1,
                 limit_enviro2,
                 predation,
                 k_c,
                 Ca,
                 Cj,
                 conv_adu_C,
                 juv_growth_C,
                 predation_C,
                 k_n, Na, Nj,
                 rfonc_tot_test,
                 rfonc_P_Na_test,
                 rfonc_P_Nj_test,
                 surplus_NRJ_test,
                 rfonc_P_Ma_test,
                 rfonc_P_Mj_test,
                 rfonc_P_Ma_Messier,
                 rfonc_P_Mj_Messier,
                 rep_fonc_Messier,
                 rep_fonc_turchin,
                 surplus_NRJ_Messier,
                 k_P_Messier,
                 wolf_decrease,
                 Pj,
                 growth_young_wolf,
                 Ma_supplementary_NRJ,
                 chi_M,
                 Ma_supplementary_NRJ_test,
                 Na_supplementary_NRJ,
                 Ca_supplementary_NRJ,
                 k_P_Johnson,
                 ungulate_biomass
                 # Qa,
                 # Qj
    ))
  })
}
