# Modelisation de l'evolution de la vegetation en fct 
# des valeurs initiales et d'une perturbation

  # Calcul les evolutions des capacites de charge en fct 
  # de PP et du temps
evol_vg <- function(t, y){

  # t = 1500
  ##Perturbation :
  # # Definition du moment de la perturbation
  t_pertub = 100            # years
  # #t_pertub = 0
  
  # Definition des moments suivant la perturbation
  t_low = t_pertub + 5           # years : temps pour atteindre le minimum de
  # biomasse apres une perturbation
  t_kpeak = t_low + (50 - 25*PP) # years :  temps pour atteindre le maximum de
  # biomasse apres une perturbation
  t_kstable = t_kpeak + 100      # years : temps pour atteindre une stabilité de
  # biomasse apres une perturbation
  
  
  # ==============================================================================
    # ------------- Variations de k_U, k_W et k_V en fonction du temps  ------------
    
    # Coefficients for the linear variation of k_U and k_W between t_peak and
  # t_stable
  kUcoeff1 = (kUstable - kUpeak)/(t_kstable - t_kpeak)
  kUcoeff2 = kUpeak - (kUstable - kUpeak)/(t_kstable - t_kpeak) * t_kpeak
  kWcoeff1 = (kWstable - kWpeak)/(t_kstable - t_kpeak)
  kWcoeff2 = kWpeak - (kWstable - kWpeak)/(t_kstable - t_kpeak) * t_kpeak
  
  
   if (t_pertub <= t & t < t_low) {
    # k_U = 0
    # k_W = 0
    # k_V = 0
    k_U = kUlow
    k_W = kWlow
    k_V = kVlow
  } else if (t_low <= t & t < t_kpeak) {
    # k_U = 1
    # k_W = 1
    # k_V = 1
    k_U = kUpeak
    k_W = kWpeak
    k_V = kVnorm
  } else if (t_kpeak <= t & t <= t_kstable) {
    # k_U = 1.1
    # k_W = 1.1
    # k_V = 1.1
    k_U = kUcoeff1*t + kUcoeff2
    k_W = kWcoeff1*t + kWcoeff2
    k_V = kVnorm
  } else {
    # k_U = 1.3
    # k_W = 1.3
    # k_V = 1.3
    k_U = kUstable
    k_W = kWstable
    k_V = kVnorm
  }
  # return(list(c(k_U,k_W,k_V)))
  res <- c(k_U,k_W,k_V)
  
  
}

# b <- evol_vg(1,y0)
# a <- evol_vg(100,y0)
