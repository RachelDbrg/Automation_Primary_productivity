# Call the function computing vegetation parameters based 
# on several PP values
source("Auto_gen_PP.R")

# Call the parameters that are assumed to not depend on PP
source("Static_vegetation_parameters.R")

# Call the function to get the data frame
result_df <- generate_parameter_dataframe()

