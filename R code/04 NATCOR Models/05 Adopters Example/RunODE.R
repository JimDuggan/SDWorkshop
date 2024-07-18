library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(tidyr)
library(readsdr)

FILE <- "R/05 Adopters Example/Adopters.stmx"
mdl        <- read_xmile(FILE)

# Check the stocks
sd_stocks(mdl)

# Check the auxiliaries
sd_constants(mdl) |> 
  mutate(value = format(value, scientific = FALSE))

meas_mdl <- list("Adoptions ~ poisson(net_flow(Total_Adopted))")

syn <- sd_measurements(n_meas       = 1,
                       ds_inputs    = mdl$deSolve_components,
                       meas_model   = meas_mdl,
                       start_time   = 0,
                       stop_time    = 20,
                       timestep     = 1/8,
                       integ_method = "euler") %>%
  as_tibble()


stan_d <- list(n_obs      = nrow(syn),
               x0         = sd_stocks(mdl)$init_value,
               Adoptions  = syn$measurement,
               t0         = 0,
               ts         = 1:nrow(syn))



mod           <- cmdstan_model("R/05 Adopters Example/stan/Adopters_F.stan")

fit <- mod$sample(data              = stan_d,
                  chains            = 4,
                  parallel_chains   = 4,
                  iter_warmup       = 1000,
                  iter_sampling     = 1000,
                  refresh           = 100,
                  save_warmup       = FALSE)

mod$expose_functions()

r_mod$functions$a_plus_b(10,20)