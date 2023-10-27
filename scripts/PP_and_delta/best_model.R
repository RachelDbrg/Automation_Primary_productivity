library(tidyverse)

setwd("C:/Users/lab/Documents/Automation_Primary_productivity/scripts")

sim1 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim1.R")
sim2 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim2.R")
sim3 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim3.R")
sim5 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim5.R")
sim6 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim6.R")
sim7 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim7.R")
sim8 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim8.R")
sim9 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim9.R")
sim10 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim10.R")
sim11 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim11.R")
sim12 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim12.R")
sim13 <- readRDS("~/Automation_Primary_productivity/Messier_figure/sim13.R")


sim1_out <- sim1 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.1,
         phi = 0.3,
         sc = "sim1")


sim2_out <- sim2 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.5,
         phi = 0.3,
         sc = "sim2")

sim3_out <- sim3 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.1,
         phi = 0.3,
         sc = "sim3")


sim5_out <- sim5 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.1,
         phi = 0.6,
         sc = "sim5")

sim6_out <- sim6 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.5,
         phi = 0.6,
         sc = "sim6")

sim7_out <- sim7 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 1,
         phi = 0.6,
         sc = "sim7")

sim8_out <- sim8 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.1,
         phi = 0.9,
         sc = "sim8")

sim9_out <- sim9 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 0.5,
         phi = 0.9,
         sc = "sim9")

sim10_out <- sim10 %>% 
  pull (outputs) %>% 
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "est",
         delta = 1,
         phi = 0.9,
         sc = "sim10")


est <-  bind_rows(sim1_out, sim2_out)
est <-  bind_rows(est, sim3_out)
est <-  bind_rows(est, sim5_out)
est <-  bind_rows(est, sim6_out)
est <-  bind_rows(est, sim7_out)
est <-  bind_rows(est, sim8_out)
est <-  bind_rows(est, sim9_out)
est <-  bind_rows(est, sim10_out)

est %>% 
  mutate(densite_messier = (58.7*(proies_tot-0.03))/(0.76+proies_tot)/1000) %>% 
  ggplot(aes(x = M_tot))










simA5 <- readRDS("~/Automation_Primary_productivity/Messier_figure/simA3.R")
simA5_test <- readRDS("~/Automation_Primary_productivity/Messier_figure/simA3_test.R")


simA5 <- simA5 %>% 
  pull(outputs) %>%
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "A5")


simA5_test <- simA5_test %>% 
  pull(outputs) %>%
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(sc = "A5test")

qwe <- bind_rows(simA5,simA5_test)

qwe %>% 
  ggplot(aes(x = time, y = M_tot))+
  geom_line() +
  facet_wrap(~sc)


qwe %>% 
  filter(time == 2000) %>% View()
