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


equa_diff_sp_test <- function(t,y,parms){
  
  # On decrit dans le vecteur `initial_values` la place
  # de chaque variable. Il faut toujours garder le même ordre.
  # En gros, on dit que pour trouver la valeur de V au temps 
  # t, il faut regarder dans la première colonne du 
  # vecteur y. etc
  
  V  <- y[1]
  U  <- y[2]
  
  with(as.list(parms), {
    
    
    # Evolution des capacites de charge de la vgtation
    # en fct des perturbations
    # capacite_vg <- evol_vg(t,y)
    k_U <- test_df[[3]]
    k_W <- test_df[[4]]
    k_V <- test_df[[5]]
    
    # # Trigger effect
    # extr_event <- trigger_fct(t,y)
    # verif <- extr_event
    
    # Lichen
    # Modèle logistique
    dVdt <- v_croiss * V * (1 - V/k_V)
    
    # Feuillus
    # Modèle exponentiel
    dUdt <- u_croiss * (1 - U/k_U)
    
    
  return (list(c(dVdt, dUdt)))
    })
}


# ----------------------------------------------------------
# NESTED + MAP VERSION

equa_diff_sp_test_nest <- function(t,y,parms){
  
  # On decrit dans le vecteur `initial_values` la place
  # de chaque variable. Il faut toujours garder le même ordre.
  # En gros, on dit que pour trouver la valeur de V au temps 
  # t, il faut regarder dans la première colonne du 
  # vecteur y. etc
  # 
  V  <- y[1]
  # U  <- y[2]
  # 
  # Unnest the df to access initial data values
  
  
  # kUstable <- changing_param[[4]]
  # kUpeak <- changing_param$kUpeak
  # kUcoeff1 <- changing_param$kUcoeff1
  # kUcoeff2 <- changing_param$kUcoeff2
  
  # changing_param <- generate_parameter_dataframe()
  # changing_param <- changing_param[1,] # A CHANGER 
  
  
  with(as.list(c(parms)), {
    
    # t_perturb <- 100
    # t_low <- changing_param[[11]]
    # t_kpeak <- changing_param[[12]]
    # t_kstable <- changing_param[[13]]
    # kUstable <- changing_param[[14]]
    # kUcoeff1 <- changing_param[[15]]
    # kUcoeff2 <- changing_param[[16]]
      
    # t_pertub = 10
    # kUstable <- result_df$kUstable
    # kUpeak <- result_df$kUpeak
    # t_kstable <- result_df$t_kstable
    # t_kpeak <- result_df$t_kpeak
    # t_low <- result_df$t_low
    
    
    # Evolution des capacites de charge de la vgtation
    # en fct des perturbations
    # capacite_vg <- evol_vg(t,y)
    # k_U <- Unnested_df[[3]]
    # k_W <- Unnested_df[[4]]
    # k_V <- Unnested_df[[5]]
    
    # capacite_vg <- evol_vg(t,y)
    # # k_U <- capacite_vg[[1]]
    # # k_W <- capacite_vg[[2]]
    # k_V <- capacite_vg[[3]]
    
    
    cap_vg <- evol_vg(data)
    k_V <- cap_vg[[1]]
    # 
    # # Trigger effect
    # extr_event <- trigger_fct(t,y)
    # verif <- extr_event
    
    # capacite_vg <- evol_vg(t,y)
    # k_U <- capacite_vg[[1]]
    
    # Lichen
    # Modèle logistique
    dVdt <- v_croiss * V * (1 - V/k_V)
    
    # Feuillus
    # Modèle exponentiel
    # dUdt <- u_croiss * (1 - U/k_U)
    
    
    # return (list(c(dVdt, dUdt)))
    return(list(c(dVdt),
                k_V))
  })
}




