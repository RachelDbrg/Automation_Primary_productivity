LVout <- ode(y=y0, tps, equa_diff_sp, parms,
             method = "rk4") %>%
  as.data.frame() %>%
  rename("lichen" = `1`,
         "feuillus" = `2`,
         "caribou_adulte" = `3`,
         "caribou_juvenile" = `4`,
         "orignal_adulte" = `5`,
         "orignal_juvenile" = `6`,
         "loup" = `7`,
         "cerf_adulte" = `8`,
         "cerf_juvenile" = `9`,
         "pref_loup_orignal_juv" = `10`,
         "pref_loup_orignal_ad" = `11`,
         "cap_charge_loup" = `12`,
         "rep_fonc_MJ" = `13`,
         "rep_fonc_MA" = `14`,
         "rep_fonc_NJ" = `15`,
         "rep_fonc_NA" = `16`,
         "rep_fonc_tot_loup" = `17`,
         "chi_P" = `18`,
         "surplus_NRJ_P" = `19`,
         "N_tot" = `20`,
         "M_tot" = `21`,
         "C_tot" = `22`,
         "proies_tot" = `23`,
         "PP" = `24`,
         "a_P" = `27`,
         "Ma_i" = `31`,
         "Mj_i" = `32`,
         "rep_fonc_M" = `33`,
         "rep_fonc_Mj" = `34`,
         "rep_fonc_Ma" = `35`,
         "denom_rep_fonc" = `36`,
         "mu_P" = `37`,
         "ma_init" = `42`,
         "na_init" = `43`,
         "ca_init" = `44`,
         "p_init" = `45`,
         "croissance_loup" = `39`,
         "k_U" = `47`) %>%
  group_by(PP) %>%
  nest()


# ------------

LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    Ingestion    <- rIng  * Prey * Predator
    GrowthPrey   <- rGrow * Prey * (1 - Prey/K)
    MortPredator <- rMort * Predator
    
    dPrey        <- GrowthPrey - Ingestion
    dPredator    <- Ingestion * assEff - MortPredator
    
    return(list(c(dPrey, dPredator)))
  })
}

pars  <- c(rIng   = 0.2,    # /day, rate of ingestion
           rGrow  = 1.0,    # /day, growth rate of prey
           rMort  = 0.2 ,   # /day, mortality rate of predator
           assEff = 0.5,    # -, assimilation efficiency
           K      = 10)     # mmol/m3, carrying capacity

yini  <- c(Prey = 1, Predator = 2)
times <- seq(0, 200, by = 1)
out   <- ode(yini, times, LVmod, pars)
summary(out)
