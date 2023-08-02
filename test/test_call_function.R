# Call the function computing vegetation parameters based 
# on several PP values
source("Auto_gen_PP_related_parameters.R")

# Call the parameters that are assumed to not depend on PP
source("Static_vegetation_parameters.R")

# source("Parametres_animaux_deers.R")

source("Static_fauna_parameters.R")

source("Evolution_vegetation.R")

source("equations_deers.R")

source("intermediate_res.R")

# Call the function to get the data frame
# initialise the values of PP-dependent parameter
# for each PP values
result_df <- generate_parameter_dataframe()

# TOUS les parametres qui sont PP-dependants: OK 

# In "result_df" are all the scenarios that I want to 
# run the ode function on. 


# Maitenant, il faut tous les parametres dont les valeurs
# dependent du temps 