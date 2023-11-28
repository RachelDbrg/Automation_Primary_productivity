# ---- Initial conditions and parameters for vegetation ----

# t_perturb = 100
t_perturb = 0

# Vegetaux
# Lichen 
# v_init = 3e5 # qte initiale
# 
# v_init = 3e5 # qte initiale

# v_init = 1e1 # qte initiale
# v_init = 1e2 # qte initiale
# v_init = 1e3 # qte initiale
# v_init = 1e4 # qte initiale
# v_init = 1e5 # qte initiale
# v_init = 3e5 # qte initiale
v_init = 5e5 # qte initiale



v_croiss = 0.06 # facteur de croissance
# k_v = 5e5 # capacite de charge max
# kVnorm =1.4*10^5 #kg/km2
# kVnorm =3.1*10^5 #kg/km2
kVnorm =2.8*10^5 #kg/km2
kVlow = 0.2*kVnorm     # kg/km², valeur pour laquelle on atteint 50# de perte
# de biomasse du lichen


# Feuillus
# u_init = 1e1
# u_init = 1e2
# u_init = 1e3
# u_init = 1e4
# u_init = 1e5
u_init = 4e5



# u_init = 1e1
# u_init = 4e9
kUlow = 2e5
kUpeak_0 = 5.6e5
# kUstable_0 = 1e6
# kUstable_0 = 2e5

# kUstable_0 = 1e5
# kUstable_0 = 1e5
# kUstable_0 = 1e5
kUstable_0 = 4e5
# kUstable_0 = 1e5
# kUstable_0 = 1e5
# kUstable_0 = 2e10

##Arbustes W :
# Stades de succession, variation selon le temps et la productivite PP
kWlow = 2250             # kg/km^2
kWpeak_0 = 9700          # kg/km^2
kWstable_0 = 4500        # kg/km^2


# Stock all the initials values of parameters of 
# vegetation in a vector
initial_conditions_vegetation <- c(v_init, u_init)
# PP_dependant_vegetation <- c(u_croiss, kUpeak, kUstable, kWpeak, kWstable)

