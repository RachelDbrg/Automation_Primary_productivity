# Script qui calcule des resultats intermediaires, utilises
# dans le script `equations.R`, comme par exemple 
# les valeurs de preference du predateur pour ses proies,
# qui dependent de leur densite, la capacite de charge du 
# predateur, 


intermediate_res <-  function(y,parms){

  
  
  # Chargement du vecteur y, contenant les densités des
  # espèces, variables dans le temps
  V  <- y[1]
  U  <- y[2]
  
  if (U <= 0){
    U = 0}
  else{
    U  <- U
  }
  
  Na <- y[3]
  
  if (Na <= 0){
    Na = 0}
  else{
    Na  <- Na
  }
  
  # print(paste0("Na=", Na))
  
  Nj <- y[4]
  
  # print(paste0("y[4]=", y[4]))
  # print(paste0("Nj=", Nj))
  
  if (Nj <= 0){
    Nj = 0}
  else{
    Nj  <- Nj
  }
  
  # if (y[4] <= 0){
  #   y[4] = 0}
  # else{
  #   y[4]  <- y[4]
  # }
  # 
  # print(paste0("y[4]=", y[4]))
  # 
  # print(paste0("Nj=", Nj))
  
  Ma <- y[5]
  
  # print(paste0("Ma_int_res=", Ma))
  
  if (Ma <= 0){
    Ma = 0}
  else{
    Ma = Ma
  }
  
  
  # print(paste("ma=", Ma))
  # test <- as.numeric(Ma)
  # print(paste("a=", test))
  
  Mj <- y[6]
  
  if (Mj <= 0){
    Mj = 0}
  else{
    Mj = Mj
  }
  
  
  # P  <- y[7]
  # 
  # 
  # if (P <= 0){
  #   P = 0}
  # else{
  #   P = P
  # }
  
  
  # Ca <- y[8]
  
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
  
  Pa  <- y[9]
  
  
  if (Pa <= 0){
    Pa = 0}
  else{
    Pa = Pa
  }
  
  
  Pj  <- y[10]
  
  if (Pj <= 0){
    Pj = 0}
  else{
    Pj = Pj
  }
  
  # print(paste0("Pj=", Pj))
  # print(paste0("Pa=", Pa))
  # 
  # # 
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
  
  
  # print(paste0("Qj=", Qj))
  # print(paste0("Qa=", Qa))
  
  # chi_M <- data$chi_M
  
  
  # Calcul des densites totales d'orignaux
  M_tot <- Ma+Mj
  # Calcul des densites totales de caribous
  N_tot <- Na+Nj
  # Calcul des densites totales de cerfs
  C_tot <- Ca+Cj
  
  # Proie factice
  # Q_tot <- Qa+Qj
  
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
  # proies_tot <- M_tot+N_tot* 0.3 + C_tot*0.2 + 2.2204e-16
  
  # proies_tot <- M_tot+N_tot* epsi_MN + C_tot*epsi_MC + Q_tot*epsi_MC + 2.2204e-16
  proies_tot <- M_tot+N_tot* epsi_MN + C_tot*epsi_MC  + 2.2204e-16
  
  
  # proies_tot <- M_tot+N_tot + C_tot + 2.2204e-16
  
  # proies_tot <- M_tot+N_tot + C_tot + 2.2204e-16
  
  
  # print(paste0("proies_tot=", proies_tot))
  # print(paste0("M_tot=", M_tot))
  # # print(paste0("M_tot=", M_tot))
  # print(paste0("N_tot* 0.3=", N_tot* 0.3))
  # print(paste0("N_tot=", N_tot))
  # print(paste0("C_tot*0.2=", C_tot*0.2))
  # 
  
  
  # Calcul des preferences du loup par espece : en fct 
  # de leur densite relative
  # pref_P_M <- M_tot/(proies_tot)
  pref_P_Ma_i <- Ma / proies_tot
  # print(paste0("pref_P_Ma_i=", pref_P_Ma_i))
  # print(paste0("Ma=", Ma))
  # print(paste0("proies_tot=", proies_tot))
  
  pref_P_Mj_i <- Mj / proies_tot
  
  # pref_P_Mj <- pref_P_Mj + pref_P_Ma
  pref_P_M <- pref_P_Ma_i + pref_P_Mj_i
  
  # Preference intra-specifique avec un forcage de 10% sur 
  # les juveniles
  # Orignaux
  pref_P_Mj <- 0.1 * pref_P_M
  pref_P_Ma <- 0.9 * pref_P_M
  
  # print(paste0("pref_P_M", pref_P_M))
  # print(paste0("pref_P_Ma", pref_P_Ma))
  # print(paste0("pref_P_Mj", pref_P_Mj))
  
  
  # Caribous
  # pref_P_N <- N_tot/(proies_tot)
  pref_P_Nj <- Nj / proies_tot
  pref_P_Na <- Na / proies_tot
  pref_P_N <- pref_P_Nj + pref_P_Na
  
  pref_P_Nj <- 0.1 * pref_P_N
  pref_P_Na <- 0.9 * pref_P_N
  
  # print(paste0("pref_P_N", pref_P_N))
  # print(paste0("pref_P_Na", pref_P_Na))
  # print(paste0("pref_P_Nj", pref_P_Nj))
  
  # print(paste0("pref_tot", pref_P_N+pref_P_M ))
  
  
  # ====
  pref_P_Nj_test <- 1.1 * Nj / proies_tot
  pref_P_Na_test <- Na / proies_tot
  pref_P_N_test <- pref_P_Nj + pref_P_Na
  
  # pref_P_Nj <- 0.1 * pref_P_N
  # pref_P_Na <- 0.9 * pref_P_N
  
  # print(paste0("pref_P_N_test", pref_P_N))
  # print(paste0("pref_P_Na_test", pref_P_Na))
  # print(paste0("pref_P_Nj_test", pref_P_Nj))
  
  # === 
  
  # Cerfs
  pref_P_Cj <- Cj / proies_tot
  pref_P_Ca <- Ca / proies_tot
  pref_P_C <- pref_P_Cj + pref_P_Ca
  
  pref_P_Cj <- 0.1 * pref_P_C
  pref_P_Ca <- 0.9 * pref_P_C
  
  # Proie factice
  # pref_P_Qj <- Qj / proies_tot
  # pref_P_Qa <- Qa / proies_tot
  # pref_P_Q <- pref_P_Qj + pref_P_Qa
  # 
  # pref_P_Qj <- 0.1 * pref_P_Q
  # pref_P_Qa <- 0.9 * pref_P_Q
  
  
  # Calcul du denominateur pour la reponse fonctionnelle
  # du loup 
  den_rfonc_P <- 1 + a_P *
    ((pref_P_Mj * h_P_Mj * Mj +
        pref_P_Ma * h_P_Ma * Ma)+
       (pref_P_Nj * h_P_Nj * Nj +
          pref_P_Na * h_P_Na * Na) + 
       (pref_P_Cj * h_P_Cj * Cj +
          pref_P_Ca * h_P_Ca * Ca))
     # +
     #   (pref_P_Qj * h_P_Cj * Qj +
     #      pref_P_Qa * h_P_Ca * Qa))
  # 
  den_rfonc_P = max(den_rfonc_P, 1e-06)
  
  
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
  
  # print(paste("rfonc_P_Ma =", rfonc_P_Ma))
  # print(paste("pref_P_Ma =", pref_P_Ma))
  # # print(paste("a_P =", a_P))
  # print(paste("Ma =", Ma))
  # print(paste("den_rfonc_P =", den_rfonc_P))
  # # print(paste("phi =", phi))
  
  
  rfonc_P_Mj <- (a_P * pref_P_Mj * Mj) / den_rfonc_P * phi
  
  rfonc_P_Na <- (a_P * pref_P_Na * Na) / den_rfonc_P * phi
  
  rfonc_P_Ma_test <- (a_P * pref_P_Ma * Ma^2) / (1 + h_P_Ma * Ma^2)
  rfonc_P_Mj_test <- (a_P * pref_P_Mj * Mj^2) / (1 + h_P_Mj * Mj^2)
  
  rfonc_P_Mj_Messier <- 1.1 * (12.3*Mj/(0.47+Mj))
  rfonc_P_Ma_Messier <- 0.9 * (12.3*Ma/(0.47+Ma))
  
  rfonc_P_Na_test <- (a_P * pref_P_Na * Na^2) / (1 + h_P_Na * Na^2)
  rfonc_P_Nj_test <- (a_P * pref_P_Nj * Nj^2) / (1 + h_P_Nj * Nj^2)
  
  rfonc_P_Nj <- (a_P * pref_P_Nj * Nj) / den_rfonc_P * phi
  rfonc_P_Cj <- (a_P * pref_P_Cj * Cj) / den_rfonc_P * phi
  rfonc_P_Ca <- (a_P * pref_P_Ca * Ca) / den_rfonc_P * phi
  
  # rfonc_P_Qj <- (a_P * pref_P_Qj * Qj) / den_rfonc_P * phi
  # rfonc_P_Qa <- (a_P * pref_P_Qa * Qa) / den_rfonc_P * phi
  # 
  
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
               # +
               #   epsi_MC * (rfonc_P_Qa +
               #                epsi_Caj * rfonc_P_Qj))
  # 
  rfonc_tot_test <- (rfonc_P_Ma_test + rfonc_P_Mj_test * epsi_Maj +
                       epsi_MN * (rfonc_P_Na_test +
                                    epsi_Naj * rfonc_P_Nj_test)+
                       epsi_MC * (rfonc_P_Ca + 
                                    epsi_Caj * rfonc_P_Cj))
  
  rep_fonc_Messier <- (rfonc_P_Ma_Messier + rfonc_P_Mj_Messier * epsi_Maj +
                         epsi_MN * (rfonc_P_Na_test +
                                      epsi_Naj * rfonc_P_Nj_test)+
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
                # +
                #   epsi_MC * (rfonc_P_Qa + rfonc_P_Qj *epsi_Caj))
  
  NRJ_intake_test = (rfonc_P_Ma_test + rfonc_P_Mj_test * (epsi_Maj)
                     + epsi_MN * (rfonc_P_Na_test + rfonc_P_Nj_test *epsi_Naj)+
                       epsi_MC * (rfonc_P_Ca + rfonc_P_Cj *epsi_Caj))
  
  NRJ_intake_Messier = (rfonc_P_Ma_Messier + rfonc_P_Mj_Messier * (epsi_Maj)
                        + epsi_MN * (rfonc_P_Na_test + rfonc_P_Nj_test *epsi_Naj)+
                          epsi_MC * (rfonc_P_Ca + rfonc_P_Cj *epsi_Caj))
  
  
  # print(paste("Ma =", Ma))
  # print(paste("P =", P))
  # print(paste("rfonc_P_Ma =", rfonc_P_Ma))
  # print(paste("rfonc_P_Mj =", rfonc_P_Mj))
  # print(paste("epsi_Maj =", epsi_Maj))
  # print(paste("epsi_MN =", epsi_MN))
  # print(paste("rfonc_P_Na =", rfonc_P_Na))
  # print(paste("rfonc_P_Ca =", rfonc_P_Ca))
  # print(paste("NRJ_intake =", NRJ_intake))
  
  # print(NRJ_intake)
  
  surplus_NRJ = NRJ_intake-mu_P
  
  surplus_NRJ_test = NRJ_intake_test-mu_P
  
  surplus_NRJ_Messier = NRJ_intake_Messier-mu_P
  
  
  # print(paste0("surplus_NRJ=", surplus_NRJ))
  # print(paste0("NRJ_intake=", NRJ_intake))
  # print(paste0("mu_P=", mu_P))
  # print(surplus_NRJ)
  
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
  if (surplus_NRJ <= 0 ){
    chi_P = 0
    surplus_NRJ = 0}
  else{
    # chi_P = surplus_NRJ * ((0.1537-0)/(6.69-2.07))
    chi_P = 0.236
  }
  
  if (surplus_NRJ_test <= 0 ){
    chi_P = 0
    # surplus_NRJ_test = 0
  }
  else{
    # chi_P = surplus_NRJ_test * ((0.1537-0)/(6.69-2.07))
    chi_P = 0.236
  }
  
  if (surplus_NRJ_Messier <= 0 ){
    chi_P = 0
    # surplus_NRJ_test = 0
  }
  else{
    # chi_P = surplus_NRJ_test * ((0.1537-0)/(6.69-2.07))
    chi_P = 0.236
  }
  
  
  
  # TO UNCOMMENT !!!!!!!!!!!!!!!!
  
  
  # chi_P = surplus_NRJ * ((0.1537-0)/(6.69-2.07))
  
  # k_P = abs(((58.7 * (rfonc_tot - 0.03)) / (0.76 + rfonc_tot))/1000)
  # k_P = abs(((58.7 * (proies_tot - 0.03)) / (0.76 + proies_tot))/1000)
  
  # 
  # k_P = 0.005
  # k_P = 0.01
  #pourrait être densité max de max
  # Wolf densities ranged from 1.6/1000 km2 in NWT to 15.6/1000 km2 in northeast BC.
  # https://www.nwt-esrf.org/sites/nesrf/files/2016-10/Human%20Footprint%2C%20Habitat%2C%20Wolves%20and%20Boreal%20Caribou%20Population%20Growth%20Rates_2016.pdf
  
  # k_P = (((58.7 * (M_tot - 0.03)) / (0.76 + M_tot))/1000)
  
  k_P_Messier = (((58.7 * (M_tot - 0.03)) / (0.76 + M_tot))/1000)
  
  
  # ungulate_biomass  = Ma*w_Ma + Mj*w_Mj + Na*w_Na + Nj*w_Nj + Ca*w_Ca + Cj*w_Cj
  ungulate_biomass  = 6*(Ma + Mj) + 2*(Na + Nj) + 1*(Ca + Cj)
  
  k_P_Johnson = 5.4 * ungulate_biomass - 0.166*ungulate_biomass^2 
  k_P = k_P_Johnson/1000
  
  
  # Conversion rates based on weight
  epsi_MN = w_Na / w_Ma
  epsi_Maj = w_Mj/w_Ma
  epsi_Naj = w_Nj/w_Na
  epsi_WC = 0.934
  epsi_MC = w_Ca / w_Ma
  epsi_Caj = w_Cj/w_Ca
  
  # 
  # k_P = 0.05
  
  # k_P = 0.05
  
  # Doesn't allow k_P to be < 0 (otherwise, leads to growth
  # of the population even if there is no preys)
  
  if (k_P <= 0){
    k_P = 0.0001
    
  } else if (k_P > 0.01){
    k_P = 0.01
    
    
  } else{
    k_P = k_P
  }
  
  
  croissance_loup = chi_P * surplus_NRJ * Pa
  
  test3 <- chi_P * 
    ((rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
        epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj))- mu_P)
  
  
  test <- p_croiss/k_P
  test1 <- (p_croiss/k_P) * Pa^2
  
  evol_P = croissance_loup - test1
  
  test4 <- rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
    epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj)- mu_P
  
  # k_m = 0.000002*U - 0.679
  # k_m = (4)^-1 * U #(Crete1989)
  
  # k_m = 0.0015 * U + 0.0131 #(Voir excel, base sur Crete 1989)
  # A l'air de donner de tres fortes densites en l'absence de predateurs
  # et des densites raisonnables sinon: OK 
  
  # k_m = 2*10^-6 * U + 0.0047
  # k_m = 5*10^-5 * U - 0.1204
  # k_m = 0.00011 * U - 0.9875
  
  k_m = 5e-6*U-1.1859
  
  
  # k_m = -0.0047 * Pa + 0.1
  # k_m = -19.953 * Pa + 2
  
  # k_m = 2
  
  # Crete 1989: https://sci-hub.se/https://doi.org/10.1139/z89-055
  # Reported densities for unhunted areas of
  # the north shore of the St. Lawrence River have never exceeded
  # 4-6 moose / 10 km2 (Crete 1987); only the adjacent Matane
  # Game Reserve, where there has been a limited hunt (Bouchard)
  
  
  if (k_m <= 0){
    k_m = 0.0047
    
  } else if (k_m > 0.6){
    k_m = 0.6
    
  } else{
    k_m  <- k_m
  }
  
  # print(k_m)
  
  # km = 2
  
  
  # print
  # print(paste0("k_m=", k_m))
  # print(paste0("U_int_res =", U))
  
  
  # k_c = 4e-5*U - 12
  # k_c = 3e-5*U - 7.9859
  k_c = 3.3294e-5*U - 7.9859
  
  # if (k_c < 0){
  #   k_c = 4}
  # else{
  #   k_c  <- k_c
  # }
  
  if (k_c <= 0){
    k_c = 0.0047
    
  } else if (k_c > 4){
    k_c = 4
    
  } else{
    k_c  <- k_c
  }
  
  # ===
  k_q = 3.3294e-5*U - 7.9859
  
  # if (k_c < 0){
  #   k_c = 4}
  # else{
  #   k_c  <- k_c
  # }
  
  if (k_q <= 0){
    k_q = 0.0047
    
  } else if (k_q > 4){
    k_q = 4
    
  } else{
    k_q  <- k_q
  }
  
  # ====
  
  Ma_supplementary_NRJ <- (((a_M * e_UM * U)/(1 + a_M * h_UM * U)) - mu_M)
  Ma_supplementary_NRJ_test <- (((a_M * U)/(1 + a_M * h_UM * U) - mu_M))
  
  
  # print(paste0("(a_M * e_UM * U)/(1 + a_M * h_UM * U)=",(a_M * e_UM * U)/(1 + a_M * h_UM * U)))
  # print(paste0("mu_M=", mu_M))
  
  
  if (Ma_supplementary_NRJ <= 0 ){
    # chi_M = 0
    Ma_supplementary_NRJ = 0
  }
  else{
    
    Ma_supplementary_NRJ <- Ma_supplementary_NRJ
  }
  
  
  # ====
  Na_supplementary_NRJ <- ((a_N * e_VN * V)/(1 + a_N * h_VN * V) - mu_N)
  
  if (Na_supplementary_NRJ <= 0 ){
    # chi_M = 0
    Na_supplementary_NRJ = 0
  }
  else{
    
    Na_supplementary_NRJ <- Na_supplementary_NRJ
  }
  
  
  # ====
  Ca_supplementary_NRJ <- ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C)
  
  if (Ca_supplementary_NRJ <= 0 ){
    # chi_M = 0
    Ca_supplementary_NRJ = 0
  }
  else{
    
    Ca_supplementary_NRJ <- Ca_supplementary_NRJ
  }
  
  # ===
  # Factice
  # Qa_supplementary_NRJ <- ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C)
  # 
  # if (Qa_supplementary_NRJ <= 0 ){
  #   # chi_M = 0
  #   Qa_supplementary_NRJ = 0
  # }
  # else{
  #   
  #   Qa_supplementary_NRJ <- Qa_supplementary_NRJ
  # }
  
  # ===
  
  # print(paste0("k_c=", k_c))
  # print(paste0("U_int_res =", U))
  
  # print(paste0("rfonc_tot_test=", rfonc_tot_test))
  # print(paste0("rfonc_P_Na_test=", rfonc_P_Na_test))
  # print(paste0("rfonc_P_Nj_test=", rfonc_P_Nj_test))
  # print(paste0("surplus_NRJ_test=", surplus_NRJ_test))
  # 
  # print(paste0("rfonc_tot=", rfonc_tot))
  # print(paste0("rfonc_P_Na=", rfonc_P_Na))
  # print(paste0("rfonc_P_Nj=", rfonc_P_Nj))
  # print(paste0("surplus_NRJ=", surplus_NRJ))
  
  wolf_decrease <- ((p_croiss/k_P) * Pa^2)
  
  
  return(list(c(rfonc_P_Mj,
                rfonc_P_Ma,
                rfonc_P_Nj,
                rfonc_P_Na,
                rfonc_P_Cj,
                rfonc_P_Ca,
                rfonc_tot),
                # rfonc_P_Qj,
                # rfonc_P_Qa),
              c(pref_P_Mj,
                pref_P_Ma,
                pref_P_Nj,
                pref_P_Na,
                pref_P_Cj,
                pref_P_Ca),
                # pref_P_Qa,
                # pref_P_Qj),
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
              rep_totale_CU,
              k_m,
              k_c,
              rfonc_tot_test,
              rfonc_P_Na_test,
              rfonc_P_Nj_test,
              surplus_NRJ_test,
              rfonc_P_Ma_test,
              rfonc_P_Mj_test,
              rfonc_P_Mj_Messier,
              rfonc_P_Ma_Messier,
              rep_fonc_Messier,
              surplus_NRJ_Messier,
              k_P_Messier,
              wolf_decrease,
              Ma_supplementary_NRJ,
              Ma_supplementary_NRJ_test,
              Na_supplementary_NRJ,
              Ca_supplementary_NRJ,
              k_P_Johnson,
              ungulate_biomass))
  
}


# Premiere liste : reponses fonctionnelles
# Deuxieme liste : preferences par espece et par statut
# Troisieme liste : autres parametres
