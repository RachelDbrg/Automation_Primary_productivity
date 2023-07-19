# ==========================================================
# ------------- Elaboration des equations ------------------

# Parametres d'entree : 
#    - t : temps
#    - y : vecteur qui prends les valeurs changeantes des sp
# y doit avoir la même taille que `initial_values`. Estimation au temps
# t des variables d'intéret 
#    - parms : vecteur qui contient toutes les valeurs
# initiales des paramètres

# Prend des valeurs itératives (qui changent à chaque "tour")


equa_diff_sp <- function(t,y,parms){
  # On decrit dans le vecteur `initial_values` la place
  # de chaque variable. Il faut toujours garder le même ordre.
  # En gros, on dit que pour trouver la valeur de V au temps 
  # t, il faut regarder dans la première colonne du 
  # vecteur y. etc
  
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
    
    k_P <- int_rlt[[4]]
    M_tot <- int_rlt [[5]]
    N_tot <- int_rlt [[6]]
    C_tot <- int_rlt[[7]]
    proies_tot <- int_rlt[[8]]
    chi_P <- int_rlt[[9]]
    surplus_NRJ <- int_rlt[[10]]
    rep_fonc_MU <- int_rlt[[11]]
    croissance_loup <- int_rlt[[12]]
    pref_P_Ma_i <- int_rlt[[13]]
    pref_P_Mj_i <- int_rlt[[14]]
    pref_P_M <- int_rlt[[15]]
    pref_P_Mj <- int_rlt[[16]]
    pref_P_Ma <- int_rlt[[17]]
    den_rfonc_P <- int_rlt[[18]]
    mu_P <- int_rlt[[19]]
    test <- int_rlt[[20]]
    test1 <- int_rlt[[21]]
    test3 <- int_rlt[[22]]
    evol_P <- int_rlt[[23]]
    test4 <- int_rlt[[24]]
    
    # Evolution des capacites de charge de la vgtation
    # en fct des perturbations
    capacite_vg <- evol_vg(t,y)
    k_U <- capacite_vg[[1]]
    k_W <- capacite_vg[[2]]
    k_V <- capacite_vg[[3]]
    
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
                 pref_P_Mj, pref_P_Ma, k_P,rfonc_P_Mj,
                 rfonc_P_Ma, rfonc_P_Nj, rfonc_P_Na, rfonc_tot,
                 chi_P, surplus_NRJ, N_tot, M_tot, C_tot,proies_tot,
                 PP, p_croiss, chi_P, a_P, 
                 mu_P, rep_fonc_MU, croissance_loup,
                 pref_P_Ma_i,
                 pref_P_Mj_i,
                 pref_P_M,
                 pref_P_Mj,
                 pref_P_Ma,
                 den_rfonc_P,
                 mu_P,
                 test,
                 test1,
                 test3,
                 evol_P,
                 ma_init,
                 na_init,
                 ca_init,
                 p_init,
                 test4,k_U,k_V))
  })
}



