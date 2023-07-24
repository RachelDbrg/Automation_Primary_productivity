
# on several PP values
source("Auto_gen_PP_related_parameters.R")


source("Static_vegetation_parameters.R")

source("Time_dependant_parameters.R")

# source("Parametres_animaux_deers.R")

source("Static_fauna_parameters.R")

# source("Evolution_vegetation.R")

source("test_model_equations.R")

source("intermediate_res.R")



# 
result_df <- generate_parameter_dataframe()

# This dataframe gives the initial values on which
# every other computation should be run


result_df <- generate_parameter_dataframe()

test <- result_df[1:3,]

nested_test <- test %>% 
  group_by(PP) %>% 
  nest()


rte()

# ----------------------------------------------------------

res <- list()

for (i in 1:nrow(test)){
  
  
  # 

    t_low <-  test[i,"t_low"]
    t_kpeak <-  test[i,"t_kpeak"]
    t_kstable <- test[i,"t_kstable"]
    kUstable <- test[i,"kUstable"]
    kUcoeff1 <- test[i,"kUcoeff1"]
    kUcoeff2 <- test[i,"kUcoeff2"]
    kUpeak <- test[i,"kUpeak"]
    
    # x <- map(parms, ~rte(.))
  
    q <- rte(t_low,
                t_kpeak,
                t_kstable,
                kUstable,
                kUcoeff1,
                kUcoeff2,
                kUpeak)
    
    # q <- as.data.frame(q)
    
    # Append the result to the results_list
    res[[i]] <- q
}
    
  # print(q)
  return(res)


# Deal with the list and maybe convert to df

library(purrr)
library(dplyr)
library(stringr)

for (i in 1:nrow(test)){
imap(res, ~ set_names(tibble(.x), .y)) %>%
  set_names(str_c("DF", 1:nrow(test))) %>% 
  list2env(.GlobalEnv)
}


