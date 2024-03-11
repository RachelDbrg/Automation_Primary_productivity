library(deSolve)
library(ggplot2)
library(purrr)

model <- function(t, y, p) {
  V <- y[1]
  U <- y[2]
  r_V <- p[[1]]
  k_V <- p[[2]]
  r_U <- p[[3]]
  k_U <- p[[4]]
  
  dV <- r_V * V * (1 - (V) / k_V)
  dU <- r_U * U * (1 - (U) / k_U)
  
  list(c(dV, dU))
  # list(c(dV))
}

# 
# p <- c(r_V = 0.05, k_V = 2.8e5, r_U = 0.03, k_U = 2e5)
# y0 <- c(V = 1e5, U = 5e4)


times <- 0:800

simulate_inout <- function(...) {
  args <- list(...)
  
  # y0_V <- args[["y0"]][1]  # Initial value for variable V
  y0_V <- args[["V_init"]][1]  # Initial value for variable V
  y0_U <- args[["U_init"]][1]  # Initial value for variable U
  p <- args[c("r_V", "k_V", "r_U", "k_U")]
  
  # print(y0_V)
  # print(y0_U)
  # print(y0)
  
  output <- ode(c(V = y0_V, U = y0_U), times, model, p)  # Using c(V, U) as initial values
  # output <- ode(c(V = y0_V), times, model, p)  # Using c(V, U) as initial values
  
  data.frame(args, output)
}

scenarios <- expand.grid(
  V_init = seq(1e5, 5e5, 1e5),
  U_init = seq(1e5, 5e5, 1e5),
  r_V = seq(0.01, 0.1, 0.01),
  # U_init = seq(1e5, 5e5, 1e5),  # Initial condition for species U
  r_U = seq(0.01, 0.1, 0.01),    # Growth rate for species U
  k_V = 2.8e5,
  k_U = 2e5  # Carrying capacity for species U
)

# 
results <- scenarios |>
  purrr::pmap(simulate_inout) |>
  list_rbind()


# results <- scenarios |> 
#   purrr::pmap(
#     ~ simulate_inout(y0 = c(..1, ..2), r_V = ..3, k_V = ..4, r_U = ..5, k_U = ..6),
#     V_init = args[["V_init"]],
#     U_init = args[["U_init"]],
#     r_V = args[["r_V"]],
#     k_V = args[["k_V"]],
#     r_U = args[["r_U"]],
#     k_U = args[["k_U"]]
#   ) |>
#   list_rbind()

results |>
  ggplot(aes(time, V)) +
  # geom_path() +
  geom_line()+
  # geom_point() +
  facet_grid(r_V ~ V_init) +
  labs(title = "Lichen growth simulations",
       subtitle = "Over several initial densities (top) and growth rates (right)")

results |>
  ggplot(aes(time, U)) +
  geom_line()+
  # geom_path() +
  facet_grid(r_U ~ U_init) +
  labs(title = "U species growth simulations",
       subtitle = "Over several initial densities (top) and growth rates (right)")



# TROP BIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIEN 
test <- scenarios |> # or %>%
  purrr::pmap(simulate_inout) |>
  list_rbind() # or dplyr::bind_rows()


# ==============================================================================
library(deSolve)
library(ggplot2)
library(purrr)

model <- function(t, y, p) {
  V <- y
  r_V <- p[[1]]
  k_V <- p[[2]]
  PP <- p[[3]]
  
  # dV <- r_V * V * (1 - V / k_V)
  dV <- r_V * V * PP * (1 - V / k_V)
  # v_croiss * V * (1 - V/k_V)
  
  list(dV)
  # return(list(dV))
}
times <- 0:100

simulate_inout <- function(...) {
  args <- list(...)
  
  y0 <- args[["V_init"]]
  p <- args[c("r_V", "k_V", "PP")]
  
  output <- ode(y0, times, model, p)
  
  data.frame(args, output, y0)
}

scenarios <- expand.grid(
  # V_init = seq(0.5, 1.5, 0.2),
  # M = seq(0.8, 1.5, 0.2),
  k_V = 2.8e5,
  # r_V = seq(0.2, 1, 0.2)
  
  V_init = seq(1e5, 5e5, 1e5),
  r_V = seq(0.01, 0.1, 0.01),
  PP = seq(0, 1, 0.1)
)

res <- scenarios |>
  purrr::pmap(simulate_inout) |>
  list_rbind() # or dplyr::bind_rows()¸


library(tidyverse)
growth_rate <- res %>%
  # first sort by year
  # arrange(year) %>%
  group_by(V_init, k_V, r_V) %>% 
  mutate(Diff_year = time - lag(time),  # Difference in time (just in case there are gaps)
         Diff_growth = X1 - lag(X1),
         Rate_percent = (Diff_growth / Diff_year)/X1 * 100) # growth rate in percent


subset <- growth_rate %>% 
  group_by(V_init, k_V, r_V) %>% 
  filter(k_V == 280000 &
         V_init == 2e+05,
         r_V == 0.01, 
         PP == 0.1)


subset %>% 
  ggplot(aes(X1, Rate_percent)) +
  geom_point()+
  facet_grid(r_V ~ V_init)


res %>%   
  ggplot(aes(time, X1, color = PP)) +
  geom_path() +
  facet_grid(r_V ~ V_init)+
  labs(title = "Lichen growth simulations without predation",
       subtitle = "Over several initial densities (top) and growth rate (right)")

res %>%   
  ggplot(aes(time, X1, color = PP)) +
  geom_path() +
  facet_grid(r_V ~ V_init)


# TROP BIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIEN 
test <- scenarios |> # or %>%
  purrr::pmap(simulate_inout) |>
  list_rbind() # or dplyr::bind_rows()


# ----
# Plot Kumpula2000

# LByn = [B / (1+(1/ (LAyn / 45)α))] + [C / (1+(1 / (LAyn / 45)α))2](2)
# 
# # where LByn is the dry matter biomass of reindeer lichens (kg d.m./ha)
# # in the nth year after forest fire in a lichen stand
# # LAyn is the age of a lichen stand (years) after forest fire (in nth year); 
# B = 4812.7364
# C = 2343.8862
# α = 4.374
# 


compute_LByn <- function(n) {
  B <- 4812.7364
  C <- 2343.8862
  alpha <- 4.374
  LAyn <- n  # Age of lichen stand in nth year
  
  term1 <- B / (1 + (1 / (LAyn / 45))^alpha)
  term2 <- (C / (1 + (1 / (LAyn / 45))^alpha))^2
  
  LByn <- term1 + term2
  return(LByn)
}

# Example usage
years <- 1:160  # Adjust the range of years as needed
results <- sapply(years, compute_LByn)
print(results)


df <- data.frame(years, results)


plot1 <- df %>% 
  ggplot(aes(x = years, 
             y = results))+
  geom_point()

plot2 <- df %>% 
  ggplot(aes(x = years, 
             y = Diff_growth))+
  geom_point()


plot1+plot2

coeff <- 1000
p1 <- df %>% 
  ggplot(aes(x = years))+
  geom_line(aes(y = Diff_growth), color = "blue")


p2 <- df %>% 
  ggplot(aes(x = years))+
  geom_line(aes(y = results), color = "red")
   # Divide by 10 to get the same range than the temperature
p2
p1+p2

df <- df %>%
  # first sort by year
  # arrange(year) %>%
  mutate(Diff_year = years - lag(years),  # Difference in time (just in case there are gaps)
         Diff_growth = results - lag(results),
         Rate_percent = (Diff_growth / Diff_year)/results * 100) # growth rate in percent


df %>% 
  ggplot(aes(x = results,
             y = Diff_growth))+
  geom_line()



compute_logistic_lichen <- function(time) {
  
  v0 <- 0.06
  k_V = 2.8 * 10^5
  
  dV <- v0 *(1 - V/k_V) *V
  
  LByn <- term1 + term2
  return(LByn)
}

# Example usage
years <- 1:160  # Adjust the range of years as needed
results <- sapply(years, compute_LByn)
print(results)


df <- data.frame(years, results)





# Logistic growth

# Load the required libraries
library(deSolve)
library(ggplot2)

# Define the differential equation
equation <- function(t, V, params) {
  with(params, {
    dVdt <- v0 * (1 - V / kV) * V
    return(list(c(dVdt)))
  })
}

# Set parameter values
params <- list(v0 = 0.1, kV = 100)

# Set initial conditions and time points
# V0 <- 10
times <- seq(0, 100, by = 0.1)

# Solve the differential equation
output <- ode(y = V0, times = times, func = equation, parms = params)

# Create a data frame
data <- data.frame(time = output[, 1], V = output[, 2])

# Create the plot
ggplot(data, aes(x = time)) +
  geom_line(aes(y = V, color = "V")) +
  scale_color_manual(values = c("V" = "blue")) +
  labs(title = "Solution of the Differential Equation",
       x = "Time",
       y = "V") +
  theme_minimal()
