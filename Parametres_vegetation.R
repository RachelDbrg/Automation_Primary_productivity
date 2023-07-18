# ---- Initial conditions and parameters for vegetation ----

# Vegetaux
# Lichen 
v_init = 5e5 # qte initiale
v_croiss = 0.06 # facteur de croissance
# k_v = 5e5 # capacite de charge max
# kVnorm =1.4*10^5 #kg/km2
# kVnorm =3.1*10^5 #kg/km2
kVnorm =2.8*10^5 #kg/km2
kVlow = 0.2*kVnorm     # kg/km², valeur pour laquelle on atteint 50# de perte
# de biomasse du lichen


# Feuillus
# u_init = 4e5
u_init = 4e5
# u_init = 4e9
u_croiss = (1+4*PP)*30000
kUlow = 2e5
kUpeak_0 = 5.6e5
kUpeak   = (1 + PP/2)*kUpeak_0
# kUstable_0 = 1e6
kUstable_0 = 2e5
kUstable = (1+(PP/2))* kUstable_0 * 2.4


##Arbustes W :
# Stades de succession, variation selon le temps et la productivite PP
w_0 = (1+4*PP)*300      # kg/(km² an), taux de recroissance initial
kWlow = 2250             # kg/km^2
kWpeak_0 = 9700          # kg/km^2
kWstable_0 = 4500        # kg/km^2
kWpeak = (1+PP/2)*kWpeak_0 # kg/km^2, pour t=t_kpeak
kWstable = (1 + PP/2)*kWstable_0 # kg/km^2, pour t=t_kstable


# Stock all the initials values of parameters of 
# vegetation in a vector
initial_conditions_vegetation <- c(v_init, u_init)


