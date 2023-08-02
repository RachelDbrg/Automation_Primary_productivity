# ------ Initial conditions and parameters for animals -----

# params_ax <- function(PP,parms_vg){
# ---- CARIBOU ----
# Initial density
# na_init = 0.68
# na_init = 2e-02
# nj_init = na_init*0.1

# na_init = 0.086
# na_init = 0.0076
na_init = 0.082755085
# nj_init = na_init*0.1

# na_init = 0
nj_init = na_init*0.1

n_croiss = 0.25  # Growth
k_n = 0.2       # Carrying capacity (for a limited 
# disturbed landscape, see Stewart et al 2020)



# FEEDING
a_N = 0.05      # Prospection/Feeding area
h_VN  = 8.81e-4 * 0.33 # Handling time
tau_n = 2 # Inverse of the time an individual is juvenile 
mu_N = 5.7467e+06 # NRJ required for maintenance
e_VN = 11.8e3 # NRJ intake

# Conversation rate of NRJ --> juveniles
chi_N = n_croiss * ((a_N * e_VN * kVnorm)
                    /(1+a_N * h_VN * kVnorm) - mu_N)^-1


# ---- MOOSE ---- 
coef = 1

# ma_init = 0

# ma_init = 0.1
# ma_init = 0.12807083025
# ma_init = 0.0896
ma_init = 0.166

# ma_init = 0.1  * coef
# ma_init = 0.00000000001

# ma_init = 0
# ma_init = 1
# ma_init = 6e-02
mj_init = ma_init*0.1

# ma_init = 0.41 # ind/km2
# mj_init = ma_init*0.1

# ma_init = 0.0
# mj_init = ma_init * 0.1

# ma_init = 4.7
# mj_init = ma_init * 0.1

m_croiss = 0.25
a_M = 0.05
h_UM  = 1.0959e-03*0.33
# k_m = (2-0.84)*PP + 0.84

k_m = ((2-0.84)*PP + 0.84) * coef

tau_m = 2 # inverse du temps où les orignaux restent 
#juveniles (estime a 6 mois)
mu_M = 1.5272e+07
# e_UM = 1.8410e+04
e_UM = mu_M * (912.5)^-1
chi_M = m_croiss * ((a_M * e_UM * kUpeak)/
                      (1+a_M * h_UM * kUpeak) - mu_M)^-1


# ---- DEER ---- 
# ca_init = 3.9
# ca_init = 2.1
ca_init = 0
# ca_init = 0.000000004
# ca_init = 0

cj_init = ca_init*0.1

# ca_init = 20
# cj_init = ca_init*0.1

h_UC = 4.1873e-03 * 0.33
a_C = 0.05
e_UC = 1.8410e+04
mu_C =  4.3967e+06
c_croiss = 0.25
chi_C = c_croiss * 
  ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1
# k_c= (11.43-4.74)*PP +4.74
k_c= ((11.43-4.74)*PP +4.74) * coef
tau_c = 2


# ---- WOLF ----
# p_init = 0.0031
# p_init = 0.004
# p_init = 0.008442148
# p_init = 0.010974793
# p_init = 0.005909504
# p_init = 0.016884297
p_init = 0.086884297

# p_init = 0.008442148


# p_init = 0.0031 * 0.5

# p_init = 0
# a_P = 65.116
# a_P = 30
a_P = 130

p_croiss = 0.36/2
mu_P = 2.0683


# Prey biomass
w_Ma = 400
w_Mj = 0.08 * w_Ma
w_Na = 100
w_Nj = 0.08 * w_Na
w_Ca = 70
w_Cj = 0.08 * w_Ca

# Conversion rates based on weight
epsi_MN = w_Na / w_Ma
epsi_Maj = w_Mj/w_Ma
epsi_Naj = w_Nj/w_Na
epsi_WC = 0.934
epsi_MC = w_Ca / w_Ma
epsi_Caj = w_Cj/w_Ca

# Handling times of preys 
h_P_Ma = 0.105
h_P_Mj = h_P_Ma * epsi_Maj
h_P_Na = h_P_Ma * epsi_MN
h_P_Nj = h_P_Na * epsi_Naj
h_P_Ca = h_P_Ma * epsi_MC
h_P_Cj = h_P_Ma * epsi_Caj


# Create a parameters vector, that stocks all the fixed 
# values of the model
parms <- c(v_croiss, kVnorm,a_P,w_Ma,w_Mj,w_Na,w_Nj,w_Cj,w_Ca,
           h_P_Ma, h_P_Mj, h_P_Na, h_P_Nj,h_P_Cj,h_P_Ca,
           epsi_Maj, epsi_MN, epsi_Naj, epsi_Caj,epsi_MC,
           mu_P, chi_M, chi_N, chi_C, p_croiss)


# Stock all the initials values of parameters of 
# animals in a vector
initial_conditions_animals <- c(na_init, nj_init,
                                ma_init, mj_init, p_init,
                                ca_init, cj_init)

# Concatenate both vegetation and animals initials values
y0 <- c(initial_conditions_vegetation
        ,initial_conditions_animals)


# parametres_ax <- as.data.frame(do.call(rbind,list(initial_conditions_animals)))
# 
# return()
# }