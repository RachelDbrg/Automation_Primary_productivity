# Script qui calcule des resultats intermediaires, utilises
# dans le script `equations.R`, comme par exemple 
# les valeurs de preference du predateur pour ses proies,
# qui dependent de leur densite, la capacite de charge du 
# predateur, 


intermediate_res <-  function(y,parms){
 
  # Ma <- 0
  # Mj <- 0
  # Ca <- 0
  # Cj <- 0
  # Na <- 0
  # Nj <- 0
  # U <- 0
  # V <- 0
  # P <- 0.004
  
  
  # Chargement du vecteur y, contenant les densités des
  # espèces, variables dans le temps
  V  <- y[1]
  U  <- y[2]
  Na <- y[3]
  Nj <- y[4]
  Ma <- y[5]
  Mj <- y[6]
  P  <- y[7]
  Ca <- y[8]
  Cj <- y[9]

  # Calcul des densites totales d'orignaux
  M_tot <- Ma+Mj
  # Calcul des densites totales de caribous
  N_tot <- Na+Nj
  # Calcul des densites totales de cerfs
  C_tot <- Ca+Cj
  
  # if (Mj < 0){
  #   Mj = 0}
  # 
  # if (Ma < 0){
  #   Ma = 0}
  # 
  # if (Na < 0){
  #   Na = 0}
  # 
  # # PROVISOIRE
  # if (any(is.na(Nj))) {
  #   Nj[is.na(Nj)] <- 0
  # }
  # 
  # if (Nj < 0) {
  #   Nj = 0}
  # 
  # 
  # if (Cj < 0){
  #   Cj = 0}
  # 
  # if (Ca < 0){
  #   Ca = 0}
  # 
  # print(Mj)
  # print(Ma)
  # print(Nj)
  # print(Na)
  
  
  # } else if (Ma < 0){
  #   Ma <- 0
  # } else if (Nj < 0){
  #   Nj <- 0
  # } else if (Na < 0){
  #   Na <- 0
  # } else if (Cj < 0){
  #   Cj <- 0
  # } else if (Ca < 0){
  #   Ca <- 0}
  
  # Mj <- pmax(Mj, 0)
  # Ma <- pmax(Ma, 0)
  # Nj <- pmax(Nj, 0)
  # Na <- pmax(Na, 0)
  # Cj <- pmax(Cj, 0)
  # Ca <- pmax(Ca, 0)
  # 
  
  #   
  
  # Calcul des densites totales de proies du systeme
  # proies_tot <- M_tot+N_tot+C_tot + 2
  # proies_tot <- M_tot+N_tot+C_tot + 2.2204e-16
  proies_tot <- M_tot+N_tot* 0.3 + C_tot*0.2 + 2.2204e-16
  
  # Calcul des preferences du loup par espece : en fct 
  # de leur densite relative
  # pref_P_M <- M_tot/(proies_tot)
  pref_P_Ma_i <- Ma / proies_tot
  pref_P_Mj_i <- Mj / proies_tot
  
  # pref_P_Mj <- pref_P_Mj + pref_P_Ma
  pref_P_M <- pref_P_Ma_i + pref_P_Mj_i
  
  # Preference intra-specifique avec un forcage de 10% sur 
  # les juveniles
  # Orignaux
  pref_P_Mj <- 0.1 * pref_P_M
  pref_P_Ma <- 0.9 * pref_P_M
  
  # Caribous
  # pref_P_N <- N_tot/(proies_tot)
  pref_P_Nj <- Nj / proies_tot
  pref_P_Na <- Na / proies_tot
  pref_P_N <- pref_P_Nj + pref_P_Na

  pref_P_Nj <- 0.1 * pref_P_N
  pref_P_Na <- 0.9 * pref_P_N
  
  # Cerfs
  pref_P_Cj <- Cj / proies_tot
  pref_P_Ca <- Ca / proies_tot
  pref_P_C <- pref_P_Cj + pref_P_Ca
  
  pref_P_Cj <- 0.1 * pref_P_C
  pref_P_Ca <- 0.9 * pref_P_C
  
    

  # Calcul du denominateur pour la reponse fonctionnelle
  # du loup 
  den_rfonc_P <- 1 + a_P *
      ((pref_P_Mj * h_P_Mj * Mj +
         pref_P_Ma * h_P_Ma * Ma)+
    (pref_P_Nj * h_P_Nj * Nj +
        pref_P_Na * h_P_Na * Na) + 
    (pref_P_Cj * h_P_Cj * Cj +
       pref_P_Ca * h_P_Ca * Ca))
  
  # print (den_rfonc_P,
  #        a_P,
  #        pref_P_Ma,
  #        Ma,
  #        scientific = TRUE)
  
  # formatted_value <- sprintf("%.10f", den_rfonc_P)
  # print(den_rfonc_P, scientific = TRUE)
  
  # Calcul des reponses fonctionnelles pour chaque espece
  # et chaque stade de vie
  rfonc_P_Ma <- (a_P * pref_P_Ma * Ma) / den_rfonc_P * phi
  rfonc_P_Mj <- (a_P * pref_P_Mj * Mj) / den_rfonc_P * phi
  rfonc_P_Na <- (a_P * pref_P_Na * Na) / den_rfonc_P * phi
  rfonc_P_Nj <- (a_P * pref_P_Nj * Nj) / den_rfonc_P * phi
  rfonc_P_Cj <- (a_P * pref_P_Cj * Cj) / den_rfonc_P * phi
  rfonc_P_Ca <- (a_P * pref_P_Ca * Ca) / den_rfonc_P * phi
  
  
  # rfonc_P_Ma <- 0
  # rfonc_P_Mj <-0
  # rfonc_P_Na <- 0
  # rfonc_P_Nj <- 0
  # rfonc_P_Cj <- 0
  # rfonc_P_Ca <-0
  
  rfonc_tot = (rfonc_P_Ma + rfonc_P_Mj * epsi_Maj +
                  epsi_MN * (rfonc_P_Na +
                            epsi_Naj * rfonc_P_Nj)+
                  epsi_MC * (rfonc_P_Ca + 
                              epsi_Caj * rfonc_P_Cj))
  
  
  # Calul de la capacite de charge de l'environnement
  # pour le loup, en fct de la densite de proies totale
  
  # Set k_P to 0 if no preys in the system
  
  # k_P = ((58.7 * (rfonc_tot - 0.03)) / (0.76 + rfonc_tot))/1000
  
  # # rfonc_tot = -1
  # if (rfonc_tot <= 0){
  # print ("pas de proies")
  #   k_P  =  0
  # }else{
  #   print("proies")
  #   k_P = ((58.7 * (rfonc_tot - 0.03)) / (0.76 + rfonc_tot))/1000
  # }

  # k_P = ((58.7 * (rfonc_tot - 0.03)) / (0.76 + rfonc_tot))/1000
  
  # print(rfonc_P_Ma)
  # print(rfonc_P_Mj)
  
  
  # if (rfonc_P_Ma < 0) {
  #   print("error")
  # }
  # 
  
  # Conversion d'NRJ en jeunes loups que si NRJ intake > 
  # NRJ depensee pour metabolisme (mu_P)
  NRJ_intake = (rfonc_P_Ma + rfonc_P_Mj * (epsi_Maj)
                + epsi_MN * (rfonc_P_Na + rfonc_P_Nj *epsi_Naj)+
                  epsi_MC * (rfonc_P_Ca + rfonc_P_Cj *epsi_Caj))
  
  
  
  surplus_NRJ = NRJ_intake-mu_P
  
  # print(NRJ_intake)
  # print(surplus_NRJ)
  
  # Add of a condition that states that if wolf don't
  # have enough NRJ to complete their basal needs, 
  # then they won't put NRJ in reproduction (so chi_P = 0)
  
  # surplus_NRJ = -2
  # chi_P = 1
  # chi_P =  surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  
  
  # if (surplus_NRJ <= 0){
  # chi_P   =   0
  # print ("Pas de nourriture")
  # 
  # }else{
  #   print ("Nourriture")
  #   # chi_P  =   surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  # }
  # res <- c(chi_P)
  # return(chi_P)
  
  # Functional response of moose on deciduous (in kg/year per moose individual)
  rep_fonc_MU = ((a_M * U)/(1 + a_M * h_UM * U))
  
  # Total response of moose on deciduous (total biomass eaten by the moose population)
  rep_totale_MU = ((a_M * U * (Ma+Mj))/(1 + a_M * h_UM * U))
  
  # Functional response of moose on deciduous (in kg/year per moose individual)
  rep_fonc_CU = ((a_C * U)/(1 + a_C * h_UC * U))
  
  # Total response of moose on deciduous (total biomass eaten by the moose population)
  rep_totale_CU = ((a_C * U * (Ca+Cj))/(1 + a_C * h_UC * U))
  
  # Not enough food so no growth
  # Set the reproductive value to 0 if wolves don't get
  # enough food
  # if (rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
  #     epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj)- mu_P < 0 ){
  #   chi_P = 0}
  # else{
  #   chi_P = surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  # }
  # 
  # if (is.na(surplus_NRJ) | surplus_NRJ < 0) {
  #   chi_P = 0
  # }else{
  #   chi_P = surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  # }
  
  # TO UNCOMMENT !!!!!!!!!!!!!!!!
  if (surplus_NRJ < 0 ){
    chi_P = 0}
  else{
    chi_P = surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  }

  # TO UNCOMMENT !!!!!!!!!!!!!!!!
  
  
  # chi_P = surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  
  # k_P = abs(((58.7 * (rfonc_tot - 0.03)) / (0.76 + rfonc_tot))/1000)
  k_P = abs(((58.7 * (proies_tot - 0.03)) / (0.76 + proies_tot))/1000)
  
  # Doesn't allow k_P to be < 0 (otherwise, leads to growth
  # of the population even if there is no preys)
  
  # if (k_P < 0){
  #   k_P = 0.0001
  # } else{
  #   k_P = k_P
  # }
   
 
  croissance_loup = chi_P * surplus_NRJ * P
  
  test3 <- chi_P * 
    ((rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
        epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj))- mu_P)
  
  
  test <- p_croiss/k_P
  test1 <- (p_croiss/k_P) * P^2
  
  evol_P = croissance_loup - test1
  
  test4 <- rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
    epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj)- mu_P
  
  
  
  return(list(c(rfonc_P_Mj,
                rfonc_P_Ma,
                rfonc_P_Nj,
                rfonc_P_Na,
                rfonc_P_Cj,
                rfonc_P_Ca,
                rfonc_tot),
              c(pref_P_Mj,
                pref_P_Ma,
                pref_P_Nj,
                pref_P_Na,
                pref_P_Cj,
                pref_P_Ca),
              den_rfonc_P,
              k_P,
              M_tot,
              N_tot,
              C_tot,
              proies_tot,
              chi_P,
              surplus_NRJ,
              rep_fonc_MU,
              croissance_loup,
              mu_P,
              test,
              test1,
              test3,
              evol_P,
              test4,
              rep_totale_MU,
              rep_fonc_CU,
              rep_totale_CU))
  
}


# Premiere liste : reponses fonctionnelles
# Deuxieme liste : preferences par espece et par statut
# Troisieme liste : autres parametres
