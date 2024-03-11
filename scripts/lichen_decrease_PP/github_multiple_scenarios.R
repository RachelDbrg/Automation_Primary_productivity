library(deSolve)
library(ggplot2)
library(purrr)

model <- function(t, y, p) {
  N <- y[[1]]
  M <- y[[2]]
  r <- p[[1]]
  K <- p[[2]]
  
  dN <- r * N * (1 - N / K)
  dM <- 0
  
  list(c(dN,dM))
  # list(c(dN))
}
times <- 0:10

simulate_inout <- function(...) {
  args <- list(...)
  
  # y0 <- args[c("N","M")]
  # y0 <- c(args[c("N")])
  # y0 <- vector(args[["N"]])
  y0 <- c("N" = args[["N"]], "M" = args[["M"]])
  print(y0)
  p <- args[c("r", "K")]
  
  output <- ode(y0, times, model, p)
  # print(args)
  data.frame(args, output)
}

scenarios <- expand.grid(
  N = seq(0.5, 1.5, 0.2),
  M = seq(0.8, 1.5, 0.2),
  K = 1,
  r = seq(0.2, 1, 0.2)
)

scenarios |> # or %>%
  purrr::pmap(simulate_inout) |>
  list_rbind() |> # or dplyr::bind_rows()
  ggplot(aes(time, N.1, color=M)) +
  geom_path() +
  facet_grid(r ~ N)


# TROP BIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIEN 
test <- scenarios |> # or %>%
  purrr::pmap(simulate_inout) |>
  list_rbind() # or dplyr::bind_rows()

