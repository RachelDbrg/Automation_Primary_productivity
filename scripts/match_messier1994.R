library(readr)
full_merged_df <- read_csv("~/Automation_Primary_productivity/res_simulations2/full_merged_df.csv")
View(full_merged_df)


# See if the final densities match with equations of Messier1994

final_densities <- full_merged_df %>% 
  filter(P!= 0,
         time == 800) %>% 
  mutate(P_expected = ((58.7*(M-0.03))/(0.76+M))/1000,
         diff = P - P_expected)


final_densities %>% 
  ggplot(aes(x = M))+
  geom_line(aes(y = P))+
  geom_line(aes(y = P_expected, color = "red"))


final_densities %>% 
  summarize(mean(diff))


final_densities %>% 
  head(5) %>% 
  ggplot(aes(x = M))+
  geom_col(aes(y = diff))

final_densities %>% 
  ggplot(aes(x = M))+
  geom_point(aes (y = diff))
