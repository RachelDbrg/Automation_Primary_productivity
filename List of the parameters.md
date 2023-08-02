
## From "auto_gen_PP_related_parameters.R"

## From "Static_vegetation_parameters.R"

### Lichen
**v_init**: lichen initial density
**v_croiss:** growth rate
**kVnorm:** average carrying capacity
**kVlow**: lowest carrying capacity, after a perturbation

### Deciduous
**u_init**: deciduous initial density
**v_croiss:** growth rate
**kUstable_0:** average carrying capacity
**kUlow**: lowest carrying capacity, after a perturbation
**kUpeak_0**: highest carrying capacity, after a perturbation

### Shrubs
**kWstable_0:** average carrying capacity
**kWlow**: lowest carrying capacity, after a perturbation
**kWpeak_0**: highest carrying capacity, after a perturbation




## From "Static_fauna_parameters.R"

### Caribou
**na_init:** adult caribou initial density
**nj_init:** young caribou initial density
**n_croiss**: intrinsic growth rate
**k_n:** Carrying capacity (for a limited disturbed landscape)
**chi_N:** conversion rate of energy into youngs
**w_Na:** weight of an adulte
**w_Nj** = weight of a young
**epsi_MN:** conversion rates for wolf, based on the moose
**epsi_Naj:** conversion rates between adult and young
#### Feeding
**a_N:** Prospection/Feeding area
**h_VN:** Handling time
**tau_n:** Inverse of the time an individual is juvenile 
**mu_N:** NRJ required for maintenance
**e_VN:** conversion rate of energy provided by vegetation

### Moose
**ma_init:** adult moose initial density
**mj_init:** young moose initial density
**m_croiss**: intrinsic growth rate
**k_m:** Carrying capacity (for a limited disturbed landscape)
**chi_M:** conversion rate of energy into youngs
**w_Ma:** weight of an adulte
**w_Mj** = weight of a young
**epsi_Maj:** conversion rates between adult and young

#### Feeding
**a_M:** Prospection/Feeding area
**h_UM:** Handling time
**tau_m:** Inverse of the time an individual is juvenile 
**mu_M:** NRJ required for maintenance
**e_UM:** conversion rate of energy provided by vegetation

### Deer
**ca_init:** adult deer initial density
**cj_init:** young deer initial density
**c_croiss**: intrinsic growth rate
**k_c:** Carrying capacity (for a limited disturbed landscape)
**chi_C:** conversion rate of energy into youngs
**w_Ca:** weight of an adulte
**w_Cj** = weight of a young
**epsi_MC:** conversion rates for wolf, based on the moose
**epsi_Caj:** conversion rates between adult and young


#### Feeding
**a_C:** Prospection/Feeding area
**h_UC:** Handling time
**tau_c:** Inverse of the time an individual is juvenile 
**mu_C:** NRJ required for maintenance
**e_UC:** conversion rate of energy provided by vegetation

### Wolf
**p_init:** wolf initial density
**a_P:** Prospection/Feeding area
**p_croiss**: intrinsic growth rate
**h_P_Ma:** handling time of the adult moose for wolf
**h_P_Mj:** handling time of the juvenile moose for wolf
**h_P_Na:** handling time of the adult caribou for wolf
**h_P_Nj:** handling time of the juvenile caribou for wolf
**h_P_Ca:** handling time of the adult deer for wolf
**h_P_Cj:** handling time of the juvenile deer for wolf


## From "Evolution_vegetation.R"

**kUstable:** stable deciduous carrying capacity 
**kUpeak:** peak deciduous carrying capacity 
t_kstable 
t_kpeak 
t_low
kWstable
kWpeak 
t_perturb 
kUcoeff1 
kUcoeff2 
kWcoeff1 
kWcoeff2 <- data$kWcoeff2


## From "intermediate_res.R"

### Species density
**M_tot**: total moose density
**N_tot**: total caribou density
**C_tot**: total deer density
**proies_tot**: total prey density

**k_P**: wolf carrying capacity

### Predation preference
**pref_P_Ma_i**: wolf preference for adult moose, function of their relative density
**pref_P_Mj_i**: wolf preference for young moose, function of their relative density
**pref_P_M:** total wolf preference for moose

**pref_P_Na_i**: wolf preference for adult caribou, function of their relative density
**pref_P_Nj_i**: wolf preference for young caribou, function of their relative density
**pref_P_N:** total wolf preference for caribou

**pref_P_Ca_i**: wolf preference for adult deer, function of their relative density
**pref_P_Cj_i**: wolf preference for young deer, function of their relative density
**pref_P_C:** total wolf preference for deer

### Functional response
**den_rfonc_P**: denominator of the wolf functional response
**rfonc_P_Ma**: wolf functional response of the adulte moose
**rfonc_P_Mj**: wolf functional response of the young moose
**rfonc_P_Na**: wolf functional response of the adulte caribou
**rfonc_P_Nj**: wolf functional response of the young caribou
**rfonc_P_Ca**: wolf functional response of the adulte deer
**rfonc_P_Cj**: wolf functional response of the young deer

**rfonc_tot**: total functional response of wolf

**rep_fonc_MU**: moose functional response of deciduous vegetation

### Energetic values
**NRJ_intake**: Wolf energy intake based on prey eaten and their nutritional values
**surplus_NRJ**: exceeding energy that can be invest in reproduction (production of offspring)
**mu_P**: energy needed to maintain basal condition of wolf
**chi_P**: conversion rate of energy into youngs, for wolf
**croissance_loup**: actual addition of youngs into the wolf population (product of the chi_P x surplus_NRJ x P)


## From "make_ODE_function.R"

### Initial densities
**v_init**: lichen initial density
**U = u_init:** deciduous initial density
**Na = na_init:** adult caribou initial density
**Nj = nj_init:** young caribou initial density
**Ma = ma_init:** adult moose initial density
**Mj = mj_init:** young moose initial density
**P = p_init:** wolf initial density
**Ca = ca_init:** adult deer initial density
**Cj = cj_init:** young deer initial density



## From "Species_equations.R"
