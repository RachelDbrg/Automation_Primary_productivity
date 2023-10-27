library(readr)
library(tidyverse)
library(viridis)

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")


# Load the df
sim1 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim1.R")
sim2 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim2.R")
sim3 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim3.R")
sim4 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim4.R")
sim5 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim5.R")
sim6 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim6.R")
sim7 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim7.R")
sim8 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim8.R")
sim9 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim9.R")
sim10 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim10.R")
sim11 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim11.R")
sim12 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim12.R")
sim13 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim13.R")
sim14 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim14.R")
sim15 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim15.R")
sim16 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim16.R")
sim17 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim17.R")
sim18 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim18.R")
sim19 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim19.R")


sim20 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim20.R")
sim21 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim21.R")
sim22 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim22.R")

sim22_modif <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim22_modif.R")


sim23 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim23.R")

sim24 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim24.R")
sim24_modif <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim24_modif.R")
sim24_modif2 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim24_modif2.R")


sim25 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim25.R")
sim25_modif <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim25_modif.R")

# Ont besoin de simulations + longues car systemes - stables
sim26 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim26.R")
sim26_modif <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim26_modif.R")

sim27 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim27.R")
sim28long <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim28_long.R")
sim29 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim29.R")
sim30long <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim30_long.R")



sim1_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim1_west.R")
sim2_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim2_west.R")
sim3_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim3_west.R")
# sim4_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim4_west.R")
sim5_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim5_west.R")
sim6_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim6_west.R")
sim7_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim7_west.R")
sim8_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim8_west.R")
sim9_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim9_west.R")
sim10_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim10_west.R")
sim11_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim11_west.R")
sim12_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim12_west.R")
sim13_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim13_west.R")

sim14_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim14_west.R")
sim15_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim15_west.R")
sim16_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim16_west.R")
sim17_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim17_west.R")
sim18_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim18_west.R")
sim19_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim19_west.R")
sim20_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim20_west.R")



sim1_out <- sim1 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est")


sim1_init <- sim1 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim1_out_west <- sim1_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim1_init_west <- sim1 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim2_out <- sim2 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim2_init <- sim2 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)



sim2_out_west <- sim2_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim2_init_west <- sim2_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)



sim3_out <- sim3 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim3_init <- sim3 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim3_out_west <- sim3_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim3_init_west <- sim3_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)

sim4_out <- sim4 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim4_init <- sim4 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim4_out_west <- sim4_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim4_init_west <- sim4_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)

sim5_out <- sim5 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim5_init <- sim5 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim5_out_west <- sim5_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim5_init_west <- sim5_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)

sim6_out <- sim6 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim6_init <- sim6 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim6_out_west <- sim6_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim6_init_west <- sim6_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim7_out <- sim7 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim7_init <- sim7 %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim7_out_west <- sim7_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim7_init_west <- sim7_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim8_out_west <- sim8_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim8_init_west <- sim8_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim9_out_west <- sim9_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim9_init_west <- sim9_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim10_out_west <- sim10_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim10_init_west <- sim10_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim11_out_west <- sim11_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim11_init_west <- sim11_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)


sim12_out_west <- sim12_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim12_init_west <- sim12_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)

sim13_out_west <- sim13_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim13_init_west <- sim13_west %>% 
  pull (data) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)

sim14_out_west <- sim14_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")

sim15_out_west <- sim15_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")

sim16_out_west <- sim16_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")

sim17_out_west <- sim17_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")

sim18_out_west <- sim18_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")

sim19_out_west <- sim19_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")


sim20_out_west <- sim20_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "west")



sim8_out <- sim8 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim9_out <- sim9 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim10_out <- sim10 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim11_out <- sim11 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim12_out <- sim12 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim13_out <- sim13 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim14_out <- sim14 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim15_out <- sim15 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim16_out <- sim16 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim17_out <- sim17 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim18_out <- sim18 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim19_out <- sim19 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")




sim20_out <- sim20 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim21_out <- sim21 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim22_out <- sim22 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim22_modif_out <- sim22_modif %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est_modif")

sim23_out <- sim23 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim24_out <- sim24 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim24_modif_out <- sim24_modif %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est_modif")

sim24_modif2_out <- sim24_modif2 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est_modif")

sim25_out <- sim25 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim25_modif_out <- sim25_modif %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est_modif")

sim26_out <- sim26 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")

sim26_modif_out <- sim26_modif %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est_modif")

sim27_out <- sim27 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim28_out <- sim28long %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim29_out <- sim29 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")


sim30_out <- sim30long %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric)%>% 
  mutate(zone = "est")



# ==============================================================================

sim1_df <- sim1_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim1")

sim1_df_west <- sim1_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim1_west")


sim2_df <- sim2_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim2")

sim2_df_west <- sim2_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim2_west")

sim3_df <- sim3_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim3")

sim3_df_west <- sim3_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim3_west")

sim4_df <- sim4_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim4")

sim4_df_west <- sim4_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim4_west")

sim5_df <- sim5_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim5")

sim5_df_west <- sim5_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim5_west")

sim6_df <- sim6_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim6")

sim6_df_west <- sim6_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim6_west")


sim7_df <- sim7_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim7")

sim7_df_west <- sim7_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim7_west")

sim8_df_west <- sim8_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim8_west")

sim9_df_west <- sim9_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim9_west")

sim10_df_west <- sim10_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim10_west")

sim11_df_west <- sim11_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim11_west")

sim12_df_west <- sim12_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim12_west")


sim13_df_west <- sim13_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim13_west")

sim14_df_west <- sim14_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim14_west")

sim15_df_west <- sim15_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim15_west")

sim16_df_west <- sim16_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim16_west")

sim17_df_west <- sim17_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim17_west")

sim18_df_west <- sim18_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim18_west")

sim19_df_west <- sim19_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim19_west")

sim20_df_west <- sim20_out_west %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim20_west")


sim8_df <- sim8_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim8")

sim9_df <- sim9_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim9")

sim10_df <- sim10_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim10")

sim11_df <- sim11_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim11")

sim12_df <- sim12_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim12")

sim13_df <- sim13_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim13")


sim14_df <- sim14_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim14")


sim15_df <- sim15_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim15")


sim16_df <- sim16_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim16")

sim17_df <- sim17_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim17")

sim18_df <- sim18_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim18")

sim19_df <- sim19_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim19")


sim20_df <- sim20_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim20")

sim21_df <- sim21_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim21")

sim22_df <- sim22_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim22")

sim22_modif_df <- sim22_modif_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim22_modif")

sim23_df <- sim23_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim23")


sim24_df <- sim24_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim24")

sim24_modif_df <- sim24_modif_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim24_modif")

sim24_modif2_df <- sim24_modif2_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim24_modif2")


sim25_df <- sim25_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim25")

sim25_modif_df <- sim25_modif_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim25_modif")



sim26_df <- sim26_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim26")

sim26_modif_df <- sim26_modif_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim26_modif")


sim27_df <- sim27_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim27")

sim28_df <- sim28_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim28")

sim29_df <- sim29_out %>% 
  filter(time == 2000) %>% 
  mutate(sc = "sim29")

sim30_df <- sim30_out %>% 
  filter(time == 2000,
         PP <= 0.4) %>% 
  mutate(sc = "sim30")


test_est <- bind_rows(sim1_df, sim2_df)
test_est <- bind_rows(test_est, sim3_df)
test_est <- bind_rows(test_est, sim4_df)
test_est <- bind_rows(test_est, sim5_df)
test_est <- bind_rows(test_est, sim6_df)
test_est <- bind_rows(test_est, sim7_df)


# Phi = 0.6
test_est <- bind_rows(sim8_df, sim9_df)
test_est <- bind_rows(test_est, sim10_df)

test_est <- bind_rows(sim11_df, sim12_df)
test_est <- bind_rows(test_est, sim13_df)

# Phi = 1
test_est <- bind_rows(test_est, sim14_df)
test_est <- bind_rows(test_est, sim15_df)
test_est <- bind_rows(test_est, sim16_df)

# Phi = 0.3
test_est <- bind_rows(test_est, sim17_df)
test_est <- bind_rows(test_est, sim18_df)
test_est <- bind_rows(test_est, sim19_df)


# Phi = 0.6, delta different


test_est <- bind_rows(sim20_df, sim21_df)
test_est <- bind_rows(test_est, sim22_df)
test_est <- bind_rows(test_est, sim23_df)
test_est <- bind_rows(test_est, sim24_df)
test_est <- bind_rows(test_est, sim25_df)
test_est <- bind_rows(test_est, sim26_df)
test_est <- bind_rows(test_est, sim27_df)
test_est <- bind_rows(test_est, sim28_df)
test_est <- bind_rows(test_est, sim29_df)
test_est <- bind_rows(test_est, sim30_df)


test_west <- bind_rows(sim1_df_west, sim2_df_west)
test_west <- bind_rows(test_west, sim3_df_west)
# test_west <- bind_rows(test_west, sim4_df_west)
test_west <- bind_rows(test_west, sim5_df_west)
test_west <- bind_rows(test_west, sim6_df_west)
test_west <- bind_rows(test_west, sim7_df_west)
test_west <- bind_rows(test_west, sim8_df_west)
test_west <- bind_rows(test_west, sim9_df_west)
test_west <- bind_rows(test_west, sim10_df_west)
test_west <- bind_rows(test_west, sim11_df_west)
test_west <- bind_rows(test_west, sim12_df_west)
test_west <- bind_rows(test_west, sim13_df_west)


test_west <- bind_rows(sim14_df_west, sim15_df_west)
test_west <- bind_rows(test_west, sim16_df_west)
test_west <- bind_rows(test_west, sim17_df_west)
test_west <- bind_rows(test_west, sim18_df_west)
test_west <- bind_rows(test_west, sim19_df_west)
test_west <- bind_rows(test_west, sim20_df_west)


test <- bind_rows(test_est, test_west)


test %>% 
  ggplot(aes(x = PP,
             y = Ma,
             color = sc))+
  geom_point()




test %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>% 
  filter(!sc %in% c("sim25", "sim26", "sim29")) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()+
  facet_wrap(~zone, scales = "free_y")



test_west %>% 
  # mutate(phi = if_else(sc %in% c("sim1_west", "sim2_west", "sim3_west"), "low", 
                       # if_else(sc %in% c("sim5_west", "sim6_west", "sim7_west"), "medium", "high"))) %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()+
  facet_wrap(~predation_rate, scales = "free_y")


test_est %>% 
  # mutate(phi = if_else(sc %in% c("sim17", "sim18", "sim19"), "low",
  # if_else(sc %in% c("sim8", "sim9", "sim10"), "medium", "high"))) %>%
  # filter(zone=="est") %>%
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate != "taux_predation_cerf") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()+
  facet_grid(sc~predation_rate, scales = "free_y")


# Plot all species predation rate with prey density
test_est %>% 
  # mutate(phi = if_else(sc %in% c("sim17", "sim18", "sim19"), "low",
  # if_else(sc %in% c("sim8", "sim9", "sim10"), "medium", "high"))) %>%
  # filter(zone=="est") %>%
  # filter(proies_tot <= 10) %>%
  filter(!sc %in% c("sim25", "sim26", "sim29")) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  filter(predation_rate != "taux_predation_cerf") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()
  # facet_grid(sc~predation_rate, scales = "free_y")



test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  ggplot(aes(x = proies_tot,
             y = taux_predation_caribou,
             color = sc))+
  geom_point()+
  facet_wrap(~zone)
  

test_est %>%
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  ggplot(aes(x = proies_tot,
             y = taux_predation_caribou,
             color = sc))+
  geom_point()+
  facet_wrap(~sc)



# Ecart densite loups avec Messier
test_west %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(phi = if_else(sc %in% c("sim1_west", "sim2_west", "sim3_west"), "low", 
                       if_else(sc %in% c("sim5_west", "sim6_west", "sim7_west"), "medium", "high"))) %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model"))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
  facet_wrap(~phi, scales = "free_y")


test_est %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  # mutate(phi = if_else(sc %in% c("sim17", "sim18", "sim19"), "low",
  #                      if_else(sc %in% c("sim8", "sim9", "sim10"), "medium", "high"))) %>%
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model"))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
  facet_wrap(~phi, scales = "free_y")


  


# ==============================================================================
# == Evaluation du parametre phi ===============================================

# Ecart densite loups avec Messier
test_west %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(densite_messier_coeffd = ((58.7*(Ma+Mj-0.03))/(0.76+(Ma+Mj))/1000) + ((58.7*(Na+Nj-0.03))/(0.76+(Na+Nj))/1000)*0.3
         + ((58.7*(Ca+Cj-0.03))/(0.76+(Ca+Cj))/1000)*0.2) %>% 
  # mutate(phi = if_else(sc %in% c("sim5_west", "sim6_west", "sim7_west"), "0.6", 
                       # if_else(sc %in% c("sim11_west", "sim12_west", "sim13_west"), "0.7", "other"))) %>% 
  # filter(phi != "other") %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model"))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
    geom_point(aes (y = densite_messier_coeffd, color = "densite_messier_coeffd"))+
  facet_wrap(~phi, scales = "free_y")


test_west %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(phi = if_else(sc %in% c("sim5_west", "sim6_west", "sim7_west"), "0.6", 
                       if_else(sc %in% c("sim11_west", "sim12_west", "sim13_west"), "0.7", "other")),
         diff = P - densite_messier) %>% 
  group_by(phi) %>% 
  summarise(mean_diff = mean(diff))


test_est %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  # mutate(phi = if_else(sc %in% c("sim5_west", "sim6_west", "sim7_west"), "0.6", 
  # if_else(sc %in% c("sim11_west", "sim12_west", "sim13_west"), "0.7", "other"))) %>% 
  # filter(phi != "other") %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model"))+
  geom_point(aes (y = densite_messier, color = "Messier"))





# ==============================================================================
# === Get the densities values for max predation rate - west ===================

test_west_values <- test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) 

rank_caribou <- which.max(test_west_values$taux_predation_caribou)

test_west_values[rank_caribou,]
# taux_predation_caribou = 0.001526584
# Densite orignal= 0.1620465 
# Densite cerf= 1.354128  
# Densite caribou = 0.6475852 
# Loup = 0.0250522 


rank_moose <- which.max(test_west_values$taux_predation_moose)
test_west_values[rank_moose,]
# proies_tot  =  
# taux_predation_moose = 0.05706672         
# Densite orignal= 0.1640696  
# Densite cerf= 1.854431     
# Densite caribou = 1.324008  
# Loup = 0.05654266 

rank_deer <- which.max(test_west_values$taux_predation_cerf)
test_west_values[rank_deer,]
# proies_tot  =  
# taux_predation_cerf = 0.1146724         
# Densite orignal= 0.1711906 
# Densite cerf= 3.618912        
# Densite caribou = 2.246716 
# Loup = 0.07859133 

# ==============================================================================
# === Get the densities values for max predation rate - est ====================

test_est_values <- test_est %>% 
  # filter(!sc %in% c("sim25", "sim26", "sim29", "sim30")) %>% 
  # filter(zone=="west") %>% 
  # filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) 

rank_caribou <- which.max(test_est_values$taux_predation_caribou)

test_est_values[rank_caribou,]

# taux_predation_caribou = 0.01502715     
# Densite orignal= 0.1340342  
# Densite caribou = 0.3554573 
# Loup =0.01571993



rank_moose <- which.max(test_est_values$taux_predation_moose)
test_est_values[rank_moose,]

# taux_predation_moose = 0.1387775                      
# Densite orignal= 0.1299925   
# Densite caribou = 0.4009007           
# Loup= 0.02099653  


# ============================
sim20_west <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim20_west.R")

sim20long_west <- sim20_west %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est")


sim20long_west %>%  
  # filter(PP == 0) %>% 
  pivot_longer(cols = c(3:11),
               names_to = "species",
               values_to = "density") %>% 
  select(time, species, density, PP) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(species)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")



sim26<- readRDS("~/Automation_Primary_productivity/Messier_figure/sim26.R")

sim26long <- sim26 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est")


sim26long %>%  
  # filter(PP == 0) %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  select(time, species, density, PP) %>% 
  filter(!species %in% c("Ca", "Cj")) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(species)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")




# Strange stability
sim25_modif <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim25_modif.R")

sim25long <- sim25_modif %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est")


sim25long %>%  
  # filter(PP == 0) %>% 
  pivot_longer(cols = c(3:10),
               names_to = "species",
               values_to = "density") %>% 
  select(time, species, density, PP) %>% 
  filter(!species %in% c("Ca", "Cj")) %>% 
  mutate(time = as.numeric(time),
         density = as.numeric(density),
         PP = as.numeric(PP)) %>% 
  ggplot(aes(x = time,
             y = density,
             color = factor(species)))+
  geom_line()+
  facet_grid(species ~ PP,
             scales = "free")+
  labs(title = "Species density over time, for gradient PP values")



# ==============================================================================
test_modif_kp <- bind_rows(sim22_modif_df,
                           sim22_df)

test_modif_kp <- bind_rows(test_modif_kp,
                           sim24_modif_df)

test_modif_kp <- bind_rows(test_modif_kp,
                           sim24_modif2_df)

test_modif_kp <- bind_rows(test_modif_kp,
                           sim25_modif_df)

test_modif_kp <- bind_rows(test_modif_kp,
                           sim26_modif_df)



test_modif_kp %>% 
  ggplot(aes(x = proies_tot, y = k_P, color = sc))+
  geom_point()



# Ecart densite loups avec Messier
test_modif_kp %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(densite_messier_coeffd = ((58.7*(Ma+Mj-0.03))/(0.76+(Ma+Mj))/1000) + ((58.7*(Na+Nj-0.03))/(0.76+(Na+Nj))/1000)*0.3
         + ((58.7*(Ca+Cj-0.03))/(0.76+(Ca+Cj))/1000)*0.2) %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model", shape=sc))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
  geom_point(aes (y = densite_messier_coeffd, color = "densite_messier_coeffd"))



# ==============================================================================
library(tidyverse)
# Load all the datafiles for the eastern simulations

# Directory where your RDS files are located
directory <- "~/Automation_Primary_Productivity/Messier_figure/"

# List RDS files in the directory that starts with "simA"
east_files <- list.files(path = directory, pattern = "^simA", full.names = TRUE)

# Create a list to store the data
east_data_list <- list()

# Loop through each file and read it into R
for (file_path in east_files) {
  # Extract the name without the directory and extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the RDS file and store it in the list
  east_data_list[[file_name]] <- readRDS(file_path)
  
  list2env(east_data_list, envir = .GlobalEnv)
}


# List RDS files in the directory that starts with "simA"
east_data_frame_names <- ls(pattern = "^simA")

# Create a list to store the modified data frames
modified_data_frames <- list()

# Loop through each data frame and apply the operations
for (data_frame_name in east_data_frame_names) {
  # Get the data frame by its name
  data_frame <- get(data_frame_name)
  # print(data_frame_name)
  
  # Apply the operations
  modified_data_frame <- data_frame %>%
    pull(outputs) %>%
    map_dfr(as.data.frame) %>% 
    # mutate(sc = str_sub(data_frame_name, end = -14))
    mutate(sc = str_sub(data_frame_name))
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}


simA1 <- modified_data_frames[["simA1"]]
simA2 <- modified_data_frames[["simA2"]]
simA3 <- modified_data_frames[["simA3"]]
simA4 <- modified_data_frames[["simA4"]]
simA5 <- modified_data_frames[["simA5"]]
simA6 <- modified_data_frames[["simA6"]]
simA7 <- modified_data_frames[["simA7"]]
simA8 <- modified_data_frames[["simA8"]]


test_est <- do.call("rbind", mget(ls(pattern = "^simA")))


test_est <- test_est %>% 
  filter(time == 2000)



# Ecart densite loups avec Messier
test_est %>% 
  filter(proies_tot <= 10) %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(densite_messier_coeffd = ((58.7*(Ma+Mj-0.03))/(0.76+(Ma+Mj))/1000) + ((58.7*(Na+Nj-0.03))/(0.76+(Na+Nj))/1000)*0.3
         + ((58.7*(Ca+Cj-0.03))/(0.76+(Ca+Cj))/1000)*0.2) %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model", shape=sc))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
  geom_point(aes (y = densite_messier_coeffd, color = "densite_messier_coeffd"))


# ==============================================================================

# Load all the datafiles for the eastern simulations

# Directory where your RDS files are located
directory <- "~/Automation_Primary_Productivity/Messier_figure/"

# List RDS files in the directory that starts with "simA"
west_files <- list.files(path = directory, pattern = "^simB", full.names = TRUE)

# Create a list to store the data
west_data_list <- list()

# Loop through each file and read it into R
for (file_path in west_files) {
  # Extract the name without the directory and extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the RDS file and store it in the list
  west_data_list[[file_name]] <- readRDS(file_path)
  
  list2env(west_data_list, envir = .GlobalEnv)
}


# List RDS files in the directory that starts with "simA"
west_data_frame_names <- ls(pattern = "^simB")

# Create a list to store the modified data frames
modified_data_frames <- list()

# Loop through each data frame and apply the operations
for (data_frame_name in west_data_frame_names) {
  # Get the data frame by its name
  data_frame <- get(data_frame_name)
  # print(data_frame_name)
  
  # Apply the operations
  modified_data_frame <- data_frame %>%
    pull(outputs) %>%
    map_dfr(as.data.frame) %>% 
    # mutate(sc = str_sub(data_frame_name, end = -14))
    mutate(sc = str_sub(data_frame_name))
  
  # Store the modified data frame in the list
  modified_data_frames[[data_frame_name]] <- modified_data_frame
}


simB1 <- modified_data_frames[["simB1"]]
simB2 <- modified_data_frames[["simB2"]]
simB3 <- modified_data_frames[["simB3"]]
simB4 <- modified_data_frames[["simB4"]]
simB5 <- modified_data_frames[["simB5"]]
simB6 <- modified_data_frames[["simB6"]]
simB7 <- modified_data_frames[["simB7"]]
simB8 <- modified_data_frames[["simB8"]]


test_west <- do.call("rbind", mget(ls(pattern = "^simB")))


test_west <- test_west %>% 
  filter(time == 2000)



# Ecart densite loups avec Messier
test_west %>% 
  filter(proies_tot <= 10) %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  mutate(densite_messier_coeffd = ((58.7*(Ma+Mj-0.03))/(0.76+(Ma+Mj))/1000) + ((58.7*(Na+Nj-0.03))/(0.76+(Na+Nj))/1000)*0.3
         + ((58.7*(Ca+Cj-0.03))/(0.76+(Ca+Cj))/1000)*0.2) %>% 
  ggplot(aes(x = proies_tot))+
  geom_point(aes (y = P, color = "model", shape=sc))+
  geom_point(aes (y = densite_messier, color = "Messier"))+
  geom_point(aes (y = densite_messier_coeffd, color = "densite_messier_coeffd"))




test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()


test_west_values <- test_west %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot)


rank_caribou <- which.max(test_west_values$taux_predation_caribou)

test_west_values[rank_caribou,]
# proies_tot = 0.272965 
# taux_predation_caribou = 0.01511575
# Densite orignal= 0.1573003  
# Densite cerf= 0.4825425     
# Densite caribou = 0.1292664 
# Loup = 0.05923227   


rank_moose <- which.max(test_west_values$taux_predation_moose)
test_west_values[rank_moose,]
# proies_tot  =  1.555716 
# taux_predation_moose = 0.1543275                  
# Densite orignal= 0.1579987   
# Densite cerf= 1.363284     
# Densite caribou = 1.23566   
# Loup = 0.05654266 

rank_deer <- which.max(test_west_values$taux_predation_cerf)
test_west_values[rank_deer,]
# proies_tot  =  7.630208 
# taux_predation_cerf = 0.2411405         
# Densite orignal= 0.1767452  
# Densite cerf= 12.02158        
# Densite caribou = 5.172868  
# Loup = 0.1192471  



test_est %>% 
  # filter(zone=="west") %>% 
  filter(proies_tot <= 10) %>%
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot) %>% 
  pivot_longer(cols = c(taux_predation_caribou,
                        taux_predation_moose,
                        taux_predation_cerf),
               names_to = "predation_rate",
               values_to = "value") %>% 
  ggplot(aes(x = proies_tot,
             y = value,
             color = predation_rate))+
  geom_point()


test_est_values <- test_est %>% 
  # filter(zone=="west") %>% 
  # filter(proies_tot <= 10) %>% 
  mutate(tot_response_caribou = rfonc_P_Na*P,
         taux_predation_caribou = tot_response_caribou/proies_tot,
         tot_response_moose = rfonc_P_Ma*P,
         taux_predation_moose = tot_response_moose/proies_tot,
         tot_response_cerf = rfonc_P_Ca*P,
         taux_predation_cerf = tot_response_cerf/proies_tot)


rank_caribou <- which.max(test_est_values$taux_predation_caribou)

test_est_values[rank_caribou,]
# proies_tot = 0.3575076  
# taux_predation_caribou = 0.01420073         
# Densite orignal= 0.1513651  
# Densite cerf= 0  
# Densite caribou = 0.3120981      
# Loup = 0.006451505   


rank_moose <- which.max(test_est_values$taux_predation_moose)
test_est_values[rank_moose,]
# proies_tot  =  1.208429   
# taux_predation_moose = 0.2112282                          
# Densite orignal= 0.1543659   
# Densite cerf= 0     
# Densite caribou = 1.16212       
# Loup = 0.05232203   


# =========== Test of productivity effect ======================================

simA5 %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  filter(species == "Ma") %>% 
  ggplot(aes(x = time,
             y = density))+
  geom_line()+
  facet_grid(PP~species, scales="free")

simA1 <- simA1 %>% 
  mutate(sim = "A1")

simA2 <- simA2 %>% 
  mutate(sim = "A2")

test_est_test <- bind_rows(simA1, simA2)

test_est_test %>% 
  filter(time == 2000) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density,
             color=factor(sim))+
  geom_point()+
  facet_wrap(~species, scales="free"))

simA8 %>% 
  filter(time == 2000) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = PP,
             y = density))+
  geom_point()+
  facet_wrap(~species, scales="free")

simB5 %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(c(2:10),
               names_to = "species",
               values_to = "density") %>% 
  ggplot(aes(x = time,
             y = density))+
  geom_point()+
  facet_wrap(~species, scales="free")