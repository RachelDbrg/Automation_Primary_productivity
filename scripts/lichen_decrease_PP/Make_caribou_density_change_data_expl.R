

East_phi_1_test_km <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/East_phi_1_test_km.R")

dixM_N_10C_P <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/10M_N_10C_P.R")
dixM_N_P <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/10M_N_P.R")
M_N_C_P <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/M_N_C_P.R")
M_N_P <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/M_N_P.R")

dixM_N_10C_P <- dixM_N_10C_P %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "10M_N_10C_P")

dixM_N_P <- dixM_N_P %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "dixM_N_P")

M_N_C_P <- M_N_C_P %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "M_N_C_P")

M_N_P <- M_N_P %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "M_N_P")


caribou <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/Caribou.R")
moose <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/Moose.R")
caribou_moose <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/Caribou_moose.R")


caribou <- caribou %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "caribou")

moose <- moose %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "moose")


caribou_moose <- caribou_moose %>%
  pull(outputs) %>%
  map_dfr(as.data.frame) %>%
  mutate_all(as.numeric) %>%
  mutate(zone = "caribou_moose")



df_list <- list(caribou, moose, caribou_moose)


# Combine data frames using do.call and rbind
combined_df <- do.call(rbind, df_list)

combined_df %>% 
  filter(time == 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP, y = density, color = zone))+
  geom_point()+
  facet_wrap(~species, scales = "free")

# 
# East_phi_1_test_km %>%
#   mutate(tot = Mj + Ma) %>%
#   filter(time %in% c(0,10, 100)) %>% 
#   mutate(tot_C = Ca + Cj) %>% 
#   View()

# East_phi_1_test_km %>%
#   mutate(tot = Mj + Ma) %>%
#   filter(time <=25) %>% 
#   mutate(tot_C = Ca + Cj) %>% 
#   View()

# M
East_phi_1_test_km %>%
  mutate(diff = k_m - Ma+Mj,
         diff2 = k_m - Ma,
         diff3 = k_c - Ca) %>%
  pivot_longer(cols = c(2:10, k_m, diff, diff2, diff3),
               names_to = "species",
               values_to = "density") %>%
  filter(species %in% c("Ma", "Mj", "U", "k_m", "diff", "diff2")) %>%
  ggplot(aes(x = time, y = density,
             color = PP))+
  geom_line()+
  facet_grid(species~PP, scales = "free") +
  theme_minimal()


East_phi_1_test_km %>% 
  group_by(PP) %>% 
  summarize(min_U = min(U, na.rm = TRUE))

#  C
# East_phi_1_test_km %>%
#   mutate(diff = k_m - Ma+Mj,
#          diff2 = k_m - Ma,
#          diff3 = k_c - Ca) %>%
#   pivot_longer(cols = c(2:10, k_c, diff, diff2, diff3),
#                names_to = "species",
#                values_to = "density") %>%
#   filter(species %in% c("Ca", "Cj", "U", "k_c", "diff", "diff2", "diff3")) %>%
#   ggplot(aes(x = time, y = density,
#              color = PP))+
#   geom_line()+
#   facet_grid(species~PP, scales = "free") +
#   theme_minimal()



# M and C
# East_phi_1_test_km %>%
#   mutate(diff = k_m - Ma+Mj,
#          diff2 = k_m - Ma,
#          diff3 = k_c - Ca) %>%
#   pivot_longer(cols = c(2:10, k_m, diff, diff2, diff3, k_c),
#                names_to = "species",
#                values_to = "density") %>%
#   filter(species %in% c("Ma", "Mj", "U", "k_m", "diff", "diff2", "k_c", "Ca", "Cj")) %>%
#   ggplot(aes(x = time, y = density,
#              color = PP))+
#   geom_line()+
#   facet_grid(species~PP, scales = "free") +
#   theme_minimal()

# # N
# East_phi_1_test_km %>%
#   mutate(k_n = 2, 
#          diff = k_n - Na+Nj,
#          diff2 = k_n - Na) %>%
#   pivot_longer(cols = c(2:10, k_n, diff, diff2),
#                names_to = "species",
#                values_to = "density") %>%
#   filter(species %in% c("Na", "Nj", "V", "k_n", "diff", "diff2")) %>%
#   ggplot(aes(x = time, y = density,
#              color = PP))+
#   geom_line()+
#   facet_grid(species~PP, scales = "free") +
#   theme_minimal()
# 
# 
East_phi_1_test_km %>%
  filter(PP == 0.4 ) %>% View()


# all herbvivores
East_phi_1_test_km %>%
  mutate(diff = k_m - Ma+Mj,
         diff2 = k_m - Ma,
         diff3 = k_c - Ca) %>%
  pivot_longer(cols = c(2:10, k_m, diff, diff2, diff3, k_c),
               names_to = "species",
               values_to = "density") %>%
  filter(species %in% c("Ma", "Mj", "U", "k_m", "diff", "diff2", "k_c", "Ca", "Cj", "Na", "Nj")) %>%
  # filter(PP == 0.4) %>% 
  ggplot(aes(x = time, y = density,
             color = PP))+
  geom_line()+
  facet_grid(species~PP, scales = "free") +
  theme_minimal()



# ==============================================================================

# Define the folder path
folder_path <- "~/Automation_Primary_productivity/Make_caribou_move/Resume"

# List files in the folder
file_names <- list.files(folder_path, full.names = TRUE)

# Select files that start with "all_simulations_scenario"
selected_files <- file_names[grep("_PN.R$", file_names)]

# Read RDS files and store them in a list
data_list <- lapply(selected_files, readRDS)

# Filter out tibble elements from the list
tibble_elements <- data_list %>% 
  keep(is_tibble)


# Convert each item in data_list to a data frame and assign names
for (i in seq_along(tibble_elements)) {
  df_name <- paste0("PN", i)
  assign(df_name, as.data.frame(tibble_elements[[i]]))
}


# List all the objects in the environment
all_objects <- ls()

# Select the objects that match the desired pattern
selected_objects <- all_objects[grep("PN", all_objects)]

selected_objects <- list(N1, N2, N3, N4, N5, N6, N7, N8, N9, N10)

# Loop through selected data frames and apply the actions
for (i in seq_along(selected_objects)) {
  obj_name <- selected_objects[i]
  df <- get(obj_name)
  results <- df %>% 
    pull(outputs) %>% 
    map_dfr(as.data.frame) %>% 
    mutate_all(as.numeric) %>% 
    group_by(PP) %>% 
    # filter(time == 0.00 | time == 800.00) %>%
    # select(1:10, PP) %>% 
    # mutate(N = Na + Nj, M = Ma + Mj, C = Ca + Cj) %>% 
    # select(-c(4:7, "Ca", "Cj")) %>%
    mutate(scenario = i)
  
  assign(paste0("results_", obj_name), results)
}

# 
# df_list <- list(results_sc1, results_sc2, results_sc3,
#                 results_sc4, results_sc5, results_sc6,
#                 results_sc7, results_sc8, results_sc9,
#                 results_sc10)


df_list <- list(results_wo1, results_wo2, results_wo3,
                results_wo4, results_wo5, results_wo6,
                results_wo7, results_wo8, results_wo9,
                results_wo10, results_wo11)


df_list <- list(results_wo_P1, results_wo_P2, results_wo_P3,
                results_wo_P4, results_wo_P5, results_wo_P6,
                results_wo_P7, results_wo_P8, results_wo_P9,
                results_wo_P10)

df_list <- list(results_N1, results_N2, results_N3, 
                results_N4, results_N5, results_N6,
                results_N7, results_N8, results_N9, 
                results_N10)

df_list <- list(results_PN1, results_PN2, results_PN3, 
                results_PN4, results_PN5, results_PN6,
                results_PN7, results_PN8, results_PN9, 
                results_PN10)


# Combine data frames using do.call and rbind
combined_df <- do.call(rbind, df_list)



combined_df %>% 
  filter(time == 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Na") %>%
  ggplot(aes(x = PP, y = density, color = as.factor(scenario)))+
  geom_point()+
  facet_grid(species~scenario, scales = "free")

combined_df %>% 
  filter(PP == 0, time == 500) %>% 
  group_by(scenario)

combined_df %>% 
  filter(time == 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Na") %>%
  ggplot(aes(x = PP, y = density, color = scenario))+
  geom_point()+
  facet_grid(species~scenario, scales = "free")


combined_df %>% 
  # filter(time == 500) %>%
  filter(PP == 0) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Na") %>%
  ggplot(aes(x = time, y = density, color = as.factor(scenario)))+
  geom_point()+
  facet_wrap(~scenario, scales = "free")


combined_df %>% 
  group_by(scenario) %>% 
  filter(time == 500, PP == 0) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Na") %>% View()

results_wo_P7 %>% 
  # dplyr::filter(time == 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  
  ggplot(aes(x = time, y = density, color = as.factor(PP)))+
  geom_line()+
  facet_wrap(~species, scales = "free")


results_wo_P8 %>% 
  # dplyr::filter(time == 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  
  ggplot(aes(x = time, y = density, color = as.factor(PP)))+
  geom_line()+
  facet_wrap(~species, scales = "free")



res_wo7 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woG.R")

res_wo7 <- res_wo7 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


res_wo7_wo_P <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woG_wo_P.R")

res_wo7_wo_P <- res_wo7_wo_P %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


res_wo7_wo_P %>% 
  # dplyr::filter(time == 500) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  
  ggplot(aes(x = time, y = density, color = as.factor(PP)))+
  geom_line()+
  facet_wrap(~species, scales = "free")


test <- rbind(results_N7, results_N6)

test %>% 
  # filter(time == 500) %>%
  filter(PP == 0) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("Na", "P")) %>%
  ggplot(aes(x = time, y = density, color = as.factor(scenario)))+
  geom_point()+
  facet_wrap(~species, scales = "free")




nb_killed <- combined_df %>% 
  # filter(P != 0) %>% 
  mutate(nb_prey_consumed = rfonc_P_Na * P) %>% 
  group_by(scenario) %>% 
  # select(c(n_init, p_init)) %>% 
  summarise(sum_killed_prey = sum(nb_prey_consumed), .keep = "all")


init <- combined_df %>% 
  filter(time == 0, PP == 0) %>% 
  group_by(scenario)


result <- nb_killed %>%
  left_join(init, by = "scenario")


result %>% 
  ggplot(aes(x = na_init, y = sum_killed_prey)) +
  geom_point()



combined_df %>% 
  filter(PP == 0) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("Na", "P")) %>% 
  ggplot(aes(time, rfonc_P_Na))+
  geom_line()+
  facet_wrap(~scenario, scales = "free")



# =====
# compar_rep_fonc 

modif <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woK_PN.R")
classique <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woB_PN.R")


sim1 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAA_MP.R")
sim2 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAB_MP.R")
sim3 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAC_MP.R")
sim4 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAD_MP.R")
sim5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAE_MP.R")
sim6 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAF_MP.R")
sim7 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAG_MP.R")
sim8 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAH_MP.R")
sim9 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAI_MP.R")
sim10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAJ_MP.R")
sim11 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAK_MP.R")
sim12 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAL_MP.R")
sim13 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAM_MP.R")
sim14 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAN_MP.R")
sim15 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAO_MP.R")
sim16 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAP_MP.R")
sim17 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAQ_MP.R")
sim18 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAP_MP.R")
sim19 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAR_MP.R")
sim20 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAS_MP.R")
sim21 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woAT_MP.R")

sim5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woE_modif.R")
sim6 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woF_modif.R")
sim7 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woG_modif.R")
sim8 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woH_modif.R")
sim9 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woK_modif.R")

sim1 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBA_MP.R")




sim1 <-sim1 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim1") 

sim2 <-sim2 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim2") 


sim4 <-sim4 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim4") 

sim5 <-sim5 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim5") 

sim6 <-sim6 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim6") 

sim7 <-sim7 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim7") 

sim8 <-sim8 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim8") 

sim9 <-sim9 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim9") 

sim10 <-sim10 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim10") 

sim11 <-sim11 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim11")

sim12 <-sim12 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim12") 

sim13 <-sim13 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim13") 

sim14 <-sim14 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim14") 

sim15 <-sim15 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim15") 

sim16 <-sim16 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim16") 

sim17 <-sim17 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim17")

sim18 <-sim18 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim18") 

sim19 <- sim19 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim19") 

sim20 <- sim20 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim20") 

sim21 <- sim21 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim21") 

classique <-classique %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "classique")


merged <- rbind(sim1, sim3)
merged <- rbind(sim2, sim3)
merged <- rbind(sim3, sim4)
merged <- rbind(sim4, sim5)
merged <- rbind(merged, sim6)
merged <- rbind(merged, sim7)
merged <- rbind(merged, sim8)
merged <- rbind(merged, sim9)
merged <- rbind(sim10, sim11)
merged <- rbind(merged, sim12)
merged <- rbind(merged, sim13)
merged <- rbind(merged, sim14)
merged <- rbind(sim14, sim15)
merged <- rbind(merged, sim16)
merged <- rbind(sim17, sim18)

merged <- rbind(sim20, sim21)


sim3 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBC_MP.R")
sim4 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBD_MP.R")
sim5 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBE_MP.R")
sim6 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBF_MP.R")
sim7 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBG_MP.R")
sim8 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBH_MP.R")
sim9 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBI_MP.R")
sim10 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBJ_MP.R")
sim11 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBK_MP.R")
sim12 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBL_MP.R")
sim12 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBM_MP.R")
sim13 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBN_MP.R")
sim14 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woBO_MP.R")


sim6 <-sim6 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim6") 

sim7 <-sim7 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim7") 

sim8 <-sim8 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim8") 

sim9 <-sim9 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim9") 

sim10 <-sim10 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim10") 

sim11 <-sim11 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim11") 

sim12 <-sim12 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim12") 

sim14 <-sim14 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim14") 

sim5 <-sim5 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim5") 

sim5_data <-sim12 %>% 
  pull(data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "sim6") 

sim5_data %>% 
  mutate(PP = c(seq(0,1,by=0.1))) %>% 
  ggplot(aes(x = PP, y = u_croiss)) +
  geom_point()

merged <- bind_rows(sim6, sim7)
merged <- bind_rows(sim9, sim10)

sim10 %>%   
  # filter(time >= 50) %>%
  mutate(diff_P = k_P - P) %>% 
  pivot_longer(cols = c(2:12, k_m),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("k_m", "Ma", "U")) %>% 
  ggplot(aes(time, density, color = sc))+
  geom_line()+
  facet_grid(species~PP, scales = "free")


sim6 %>% 
  filter(time == 300) %>% 
  # pivot_longer(cols = c(2:12),
  # names_to = "species",
  # values_to = "density") %>% 
  # filter(species %in% c("Pa", "Pj", "Ma")) %>% View()
  ggplot(aes(Ma, U, color = PP))+
  geom_point()



sim10 %>% 
  mutate(loup_growth = chi_P * surplus_NRJ_Messier * P)


sim15 %>%
  filter(PP ==0) %>% 
  ggplot(aes(Ma, P, color = sc))+
  geom_point()+
  facet_grid(species~PP, scales = "free")

merged %>% 
  group_by(sc) %>% 
  filter(time == 300, PP == 0) %>% 
  pivot_longer(cols = c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Na") %>%
  ggplot(aes(sc, density ))+
  geom_point()


sim5 %>% 
  # group_by(sc) %>% 
  filter(time == 800) %>% 
  select(c(P, k_P,k_P_Messier))


sim9 %>% 
  filter(time == 100) %>% 
  select(c(PP, U, k_U))


min_value_F <- min(sim2$Na)
min_value_C <- min(sim1$Na)


merged %>%   
  filter(time >= 5) %>%
  mutate(diff_P = k_P - P) %>% 
  pivot_longer(cols = c(2:12, k_m),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("Ma", "U", "k_m")) %>% 
  ggplot(aes(time, density, color = sc))+
  geom_line()+
  facet_grid(species~PP, scales = "free")


sim14 %>% 
  group_by(PP) %>% 
  # filter(time <= 5) %>%
  pivot_longer(cols = c(2:12, k_m, Ma, Pa, Pj),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("U", "k_m", "Ma", "Mj", "Pa", "Pj")) %>% 
  ggplot()+
  geom_line(aes(x = time, y = density, color = factor(PP)))+
  facet_wrap(~species, scales = "free")



sim14 %>% 
  group_by(PP) %>% 
  filter(time == 300) %>% 
  select(c(PP, Ma, P, M_tot, k_m)) %>% View()
ggplot(aes(x = PP, y = Ma)) +
  geom_point()




# ======
sim1 <- readRDS("~/Automation_Primary_productivity/Final_plots/N.R")
sim2 <- readRDS("~/Automation_Primary_productivity/Final_plots/NP.R")
sim3 <- readRDS("~/Automation_Primary_productivity/Final_plots/NPM.R")
sim4 <- readRDS("~/Automation_Primary_productivity/Final_plots/NPMC.R")
sim5 <- readRDS("~/Automation_Primary_productivity/Final_plots/M.R")
sim6 <- readRDS("~/Automation_Primary_productivity/Final_plots/MP.R")
sim7 <- readRDS("~/Automation_Primary_productivity/Final_plots/MPC.R")
sim8 <- readRDS("~/Automation_Primary_productivity/Final_plots/C.R")
sim9 <- readRDS("~/Automation_Primary_productivity/Final_plots/CP.R")
sim10 <- readRDS("~/Automation_Primary_productivity/Final_plots/CPN.R")
sim11 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woEL_MP.R")
sim12 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woER_MP.R")
sim13 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woES_MP.R")
sim14 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woET_MP.R")
sim15 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woEU_MP.R")
sim16 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woEV_MP.R")
sim17 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woEZ_MP.R")
sim18 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woEX_MP.R")
# sim12 <- readRDS("~/Automation_Primary_productivity/LHS/OAAT/bs1.RDS")
sim13 <- readRDS("~/Automation_Primary_productivity/Make_caribou_move/Resume/woCM_MP.R")

library(tidyverse)
sim1 <-sim1 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "N") 

sim2 <-sim2 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "NP") 


sim3 <-sim3 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "NPM") 

sim4 <- sim4 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "NPMC") 

sim5 <-sim5 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "M") 

sim6 <-sim6 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "MP") 

sim7 <-sim7 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "MPC") 


sim8 <-sim8 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "C") 

sim9 <-sim9 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "CP") 

sim10 <-sim10 %>% 
  pull(outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "CNP") 



merged <- bind_rows(sim1, sim2)
merged <- bind_rows(merged, sim3)
merged <- bind_rows(merged, sim4)
merged <- bind_rows(merged, sim5)
merged <- bind_rows(merged, sim6)
merged <- bind_rows(merged, sim7)
merged <- bind_rows(merged, sim8)
merged <- bind_rows(merged, sim9)
merged <- bind_rows(merged, sim10)


merged %>% 
  group_by(PP) %>% 
  # filter(PP == "0.7") %>%
  # filter(sc == "sim18") |> 
  # mutate(diff_C = k_c - Ca,
  # diff_M = k_m - Ma) %>% 
  pivot_longer(cols = c(2:12, Ma, Pa, Pj, Ca, Na, "k_m", "Mj"),
               names_to = "species",
               values_to = "density") %>%
  filter(species %in% c("U", "Ca", "Pa", "Na", "Ma")) %>%
  # filter(species %in% c("Ma")) %>%
  ggplot()+
  # geom_line(aes(x = time, y = density), linewidth = 1.1)+
  geom_line(aes(x = time, y = density, color = factor(PP)), linewidth = 1.1)+
  facet_grid(species~sc, scales = "free")


merged %>% 
  # group_by(PP) %>% 
  # filter(PP == "0.7") %>%
  # filter(sc == "sim18") |> 
  # mutate(diff_C = k_c - Ca,
  # diff_M = k_m - Ma) %>% 
  pivot_longer(cols = c(2:12, Ma, Pa, Pj, Ca, Na, "k_m", "Mj"),
               names_to = "species",
               values_to = "density") %>%
  filter(species %in% c("U", "Ca", "Pa", "Na", "Ma")) %>%
  filter(time == 300) |> 
  # filter(species %in% c("Ma")) %>%
  ggplot()+
  # geom_line(aes(x = time, y = density), linewidth = 1.1)+
  geom_point(aes(x = PP, y = density, color = factor(sc)), linewidth = 1.1)+
  geom_line(aes(x = PP, y = density, color = factor(sc)))+
  facet_wrap(~species, scales = "free")



sim17 |> 
  filter(PP == "0.6" | PP == "0.7" | PP == "0.8") |> 
  select(c(PP, time, Ma, juv_growth, chi_M, Ma_supplementary_NRJ, Mj,k_m,transition_adu)) |> 
  mutate(tau_mq = tau_m,
         croiss_Ma = tau_m*Mj*(1-(Ma/k_m)),
         lim = Ma/k_m,
         lim2 = (1-(Ma/k_m)),
         verif = if_else(Ma > k_m, "neg", "pos")) |>
  View()

sim17 |> 
  filter(time >= 3 & time <=6) |> 
  filter(PP == "0.6" | PP == "0.7" | PP == "0.8") |> 
  select(c(PP, time, Ma, juv_growth, chi_M, Ma_supplementary_NRJ, Mj,k_m,transition_adu)) |> 
  mutate(tau_mq = tau_m,
         croiss_Ma = tau_m*Mj*(1-(Ma/k_m)),
         lim = Ma/k_m,
         lim2 = (1-(Ma/k_m)),
         verif = if_else(Ma > k_m, "neg", "pos")) |>
  ggplot(color = factor(PP))+
  geom_point(aes(x = time, y = Ma, color = factor(PP)))

# Plot with carrying capacities as horizontal lines
# 1. Get the CC values

# Only works for moose
k_m_values <- merged %>%
  # filter(species == "Ma") %>%
  group_by(sc, PP) %>%
  filter(time == 300) %>%
  summarise(k_m_value = unique(k_m))


carrying_capacities <- merged |> 
  filter(time == 300) |> 
  select(c(PP, sc, k_m, k_U, k_V, k_c, k_n, k_P)) |> 
  pivot_longer(cols = -c(PP, sc),
               names_to = "species",
               values_to = "value") |> 
  group_by(sc, PP) |> 
  mutate(species = case_when(
    species == "k_m" ~ "Ma",
    species == "k_U" ~ "U",
    species == "k_V" ~ "V",
    species == "k_c" ~ "Ca",
    species == "k_n" ~ "Na",
    species == "k_P" ~ "Pa",
    TRUE ~ species
  ))


# ---------------------------------
# 2. Plot them on the previous graph
merged %>%
  pivot_longer(cols = c(2:12, Ma, Pa, Pj, Ca, Na),
               names_to = "species",
               values_to = "density") %>%
  filter(species %in% c("U", "V","Ca", "Pa", "Na", "Ma", "Cj")) %>%
  ggplot() +
  geom_line(aes(x = time, y = density, color = sc), linewidth = 1.1) +
  geom_hline(data = carrying_capacities,
             aes(yintercept = value, color = sc),
             linetype = "dashed", linewidth = 1) +
  facet_grid(species ~ PP, scales = "free")






te <- merged %>% 
  filter(time == 0, PP == 0)


sim4 %>% 
  filter(time == 300) %>%

library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your data frame is named merged
merged %>%
  filter(time == 300) %>%
  # pivot_longer(cols = c(2:9),
  #              names_to = "species",
  #              values_to = "density") %>% 
  # filter(species %in% c("Na")) %>% 
  ggplot(aes(Ma, rfonc_P_Na * Pa, color = sc)) +
  geom_point() +
  geom_line()


merged %>% 
  # filter(PP == 1) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*Pa,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*Pa,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*Pa,
         taux_predation_cerf = tot_response_cerf/proies_tot,
         biomass_Moose = 400*M_tot,
         biomass_Caribou = 100*N_tot,
         biomass_Deer = 70*C_tot,
         biomass_tot = biomass_Moose+ biomass_Caribou+biomass_Deer) %>% 
  filter(biomass_tot <= 700) %>%
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate == "taux_predation_caribou") %>%
  # mutate(Zone = recode(zone, est = 'East', west = 'West')) %>% 
  ggplot(aes(x = biomass_tot,
             y = value,
             color = sc))+
  geom_point(size = 2)



sim8 %>% 
  group_by(PP) %>% 
  # filter(PP == 1) %>%
  mutate(diff_C = k_c - Ca,
         diff_M = k_m - Ma) %>% 
  pivot_longer(cols = c(2:12, Ma, Pa, Pj, Ca, Na),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("U", "Ma", "Pa", "Ca", "Na")) %>% 
  ggplot()+
  geom_line(aes(x = time, y = density, color = sc), linewidth = 1.1)+
  facet_grid(species~PP, scales = "free")

## Predator-prey plots

sim4 %>% 
  group_by(PP) %>% 
  # filter(PP == 1) %>%
  # mutate(diff_C = k_c - Ca,
  #        diff_M = k_m - Ma) %>% 
  # pivot_longer(cols = c(2:12, Ma, Pa, Pj, Ca, Na),
  #              names_to = "species",
  #              values_to = "density") %>% 
  # filter(species %in% c("U", "Ma", "Pa", "Ca", "Na")) %>% 
  ggplot()+
  geom_point(aes(x = Na, y = Pa, color = time))+
  facet_wrap(~PP, scales = "free")


sim7 %>% 
  group_by(PP) %>% 
  # filter(PP == 1) %>%
  mutate(diff_C = k_c - Ca,
         diff_M = k_m - Ma) %>% 
  pivot_longer(cols = c(2:12, Ma, Pa, Pj, Ca, Na),
               names_to = "species",
               values_to = "density") %>% 
  # filter(species %in% c("Pa")) %>% 
  ggplot()+
  geom_line(aes(x = time, y = density, color = sc))+
  facet_grid(species~PP, scales = "free")


merged_final_time <- merged %>% 
  group_by(PP) %>% 
  filter(time == 300 & !(sc %in% c("C+P_long", "N+C+P_long", "C+P", "N+C+P"))) 

merged_final_time_long <- merged %>% 
  group_by(PP) %>% 
  filter(time == 800 & (sc %in% c("C+P_long", "N+C+P_long"))) 

merged_all <- bind_rows(merged_final_time, merged_final_time_long)

merged_all%>%
  pivot_longer(cols = c(2:12, k_m, Ma, Pa, Pj, Na),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("U", "Pa", "Ma", "Na", "Ca")) %>% 
  ggplot(aes(x = PP, y = (density), color = sc))+
  geom_point()+
  geom_line(linewidth= 1.1)+
  facet_wrap(~species, scales = "free")


sim10 %>% 
  filter(time == 2000) %>% 
  pivot_longer(cols = c(2:12, k_m, Ma, Pa, Pj, Na),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("U", "k_m", "Ma", "Mj", "Pa", "Pj", "Na")) %>% 
  mutate(PP_category = cut(PP, breaks = c(0, 0.7, Inf), labels = c("Stabilised", "Pas stabilise"))) %>% 
  ggplot(aes(x = PP, y = density, color = PP_category))+
  geom_point()+
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~species, scales = "free")


merged %>% 
  group_by(PP) %>% 
  filter(time == 300) %>% 
  pivot_longer(cols = c(2:12, k_c, Ma, Pa, Pj, Na, Ca),
               names_to = "species",
               values_to = "density") %>% 
  filter(species %in% c("Ma", "Pa", "Na", "U", "k_c", "Ca")) %>%
  ggplot(aes(x = PP, y = density, color = sc)) +
  geom_line()+
  facet_grid(species~sc, scales = "free")



sim4 %>% 
  group_by(PP) %>% 
  filter(time == 300) %>% 
  select(c(PP, Na, k_n, Ma, k_m, Pa))



merged_df_wo_deer %>% 
  filter(scenario == 1) %>% 
  ggplot(aes(x = PP, y = Ma)) +
  geom_point()+
  geom_line()


sim16 |> 
  filter(time ==300) |> 
  ggplot(aes(x= ungulate_biomass, y=Pa))+
  geom_point()


sim17 |> 
  # filter(PP == 1) |> 
  filter(time == 300) |> 
  ggplot()+
  geom_point(aes(x = PP, y = Ma))+
  geom_point(aes(x = PP, y = Ca), col = "red")+
  geom_line(aes(x = PP, y = Na), col = "blue")+
  geom_point(aes(x = PP, y = k_m), col = "pink")
  
