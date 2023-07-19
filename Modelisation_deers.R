# ==========================================================
# Modelisation d'un systeme loup-ongule dans la 
# foret boreale Canadienne
# ==========================================================

setwd("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity")

#   Ceci est le script "principal", qui lit et applique
#   plusieurs autres scripts du projet.
#   A partir d'un jeu de conditions initiales, ce script
#   applique une fonction de resolution d'equations 
#   differentielles, afin de modeliser les densites de pop
#   de vegetation, d'ongule et de loup dans le temps. 


# Repertoire GitHub: 
# https://github.com/RachelDbrg/Model-ungulate-wolf


# Auteur: R. Dubourg (d'apres de P. Lamirande)

# ==========================================================

# Package dependencies
library(deSolve)
library (tidyverse)
library(data.table)


# Clear environment
rm(list = ls())

# pro = seq(-10,10, by=.01)
# pro = as.data.frame(seq(0,10, by=.01))


# SET PRODUCTIVITY FOR THE WHOLE MODEL
# Productivity
# PP = 0.7
PP = 1

# PP = 0.00001


# LENGTH OF THE SIMULATION
# Time
t0 = 0 # temps inital 
tfinal = 800 # temps final
# try simulation for shorter periods
# tfinal = 10 #years
# t_pertub = 100

# tfinal = 1 # temps final
tps <-seq (t0, tfinal, 0.25) # sequence de temps avec pas 
# de temps de 0.25


# valuesPP <- as.data.frame(seq(0,2, by=1)) %>%
#   rename(PP = "seq(0, 2, by = 1)")

# fun_func <- function(pp){
#   retval = pp*10
#   return(retval)
# }

# test <- sapply(PP,
# whole_model)

# whole_model <- function(PP){
# 1. 
# Chargement des conditions initiales
source("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity/Parametres_vegetation.R")
source("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity/Parametres_animaux_deers.R")


# Chargement des equations de croissance de la vegetation
source("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity/Evolution_vegetation.R")


# 2.
# Chargement des equations diff 
source("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity/equations_deers.R")



# Chargement des equations de reponse fonctionnelle
# source("C:/Users/radub33/OneDrive - Université Laval/Model/R/Model_ungulate-wolf/rep_fonc.R")
source("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity/intermediate_res.R")

# Chargement de la fonction qui declenche un event
# source("C:/Users/radub33/OneDrive - Université Laval/Model/R/Model_ungulate-wolf/Trigger_fire.R")


# 3. 
# Application de l'equadiff et stockage des resultats
# dans le df LVout
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


# 4.
# Sauvegarde des resultats
# saveRDS (LVout, file='res_densitesPP01.R')

# Sauvegarde des resultats, ou le nom du fichier prend
# automatiquement le format "res_densites"+valeur de PP

# Fetch de la valeur de PP et formatage
PPvalue <- gsub("\\.","",as.character(PP))

# "PP5" means PP = 5
# "PP05" means PP = 0.5
# "PP005" means PP = 0.05

# Concatenation du titre + valeur PP + extension ".R"
filename <- paste("res_densitesPP",PPvalue,sep="")

# Add initial herbivore densities in file name
init_dens <- paste(ma_init, na_init, ca_init, p_init, sep = ";")
filename <- paste(filename, init_dens, sep = "_")

# Add carrying_capacities in file name
carrying_cap <- paste(k_m, k_c, sep ="_")
filename <- paste(filename, carrying_cap, sep = "_")

filename <- paste(filename, a_P, sep = "_")

# filename <- paste(paste("res_densitesPP",PPvalue,sep=""),".R",sep="")

# Add "_WEST" if deers initial densities are not 0
if (ca_init != 0){
  filename <- paste(filename, "_WEST",sep="")
}else {
  filename <- paste(filename, "_EAST",sep="")}

filename <- paste(filename,".R",sep="")

# fp <- file.path ("C:/Users/radub33/OneDrive - Université Laval/Model/R/Model_ungulate-wolf/Simulation_results")

# fp_alt <- file.path ("C:/Users/radub33/OneDrive - Université Laval/Model/R/Model_ungulate-wolf/Simulation_results/Observed_vs_simulated")
# fp_alt <- file.path ("C:/Users/radub33/OneDrive - Université Laval/Model/R/Model_ungulate-wolf/Simulation_results/functional_response")
fp_alt <- file.path ("C:/Users/radub33/Documents/GitHub/Automation_Primary_productivity/Simulation_results")

# tr <- paste0(fp,"/", filename)

tr_alt <- paste0(fp_alt,"/", filename)


# Sauvegarde
saveRDS (LVout, file=tr_alt)

# return(LVout)
# return()
# }

# sessionInfo()

