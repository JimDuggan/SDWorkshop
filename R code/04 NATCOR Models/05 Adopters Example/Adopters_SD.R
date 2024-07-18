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

source("R code/04 NATCOR Models/05 Adopters Example/Get Pairwise.R")

FILE <- "R code/04 NATCOR Models/05 Adopters Example/Adopters.stmx"
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

ggplot(syn,aes(x=time,y=measurement))+geom_point()+geom_line()+
  theme_classic()


priors <-    list(sd_prior(par_name  = "Effective_Contacts", 
                           dist      = "lognormal", 
                           dist_pars = c(0,1)))


stan_file <- sd_Bayes(filepath         = FILE,
                      meas_mdl         = meas_mdl,
                      estimated_params = priors)

write_file(stan_file, file = "R code/04 NATCOR Models/05 Adopters Example/stan/Adopters.stan")

stan_d <- list(n_obs      = nrow(syn),
               x0         = sd_stocks(mdl)$init_value,
               Adoptions  = syn$measurement,
               t0         = 0,
               ts         = 1:nrow(syn))


mod           <- cmdstan_model("R code/04 NATCOR Models/05 Adopters Example/stan/Adopters.stan")

fit <- mod$sample(data              = stan_d,
                  chains            = 4,
                  parallel_chains   = 4,
                  iter_warmup       = 1000,
                  iter_sampling     = 1000,
                  refresh           = 100,
                  save_warmup       = FALSE)

posterior_df <- as_draws_df(fit$draws()) %>%
                   as_tibble()

p <- posterior_df %>%
  select(Effective_Contacts,log_lik) %>% 
  get_pair_wise("Inference Results")


pars_long <- posterior_df[, c("Effective_Contacts")] |>
  mutate(iter = row_number()) |> 
  pivot_longer(-iter, names_to = "par")

p_pars <- ggplot(pars_long, aes(value)) +
  geom_histogram(colour = "white", fill = "grey60", alpha = 0.75) +
  facet_wrap(vars(par), scales = "free") +
  theme_classic()

sum <- posterior_df %>%
  select(Effective_Contacts) %>%
  summarise(MeanEC=mean(Effective_Contacts),
            Q0.025=quantile(Effective_Contacts,0.025),
            Q0.975=quantile(Effective_Contacts,0.975))


ts <- posterior_df %>%
  select(starts_with("sim_Adoptions")) %>%
  summarise(MeanEC=mean(Effective_Contacts),
            Q0.025=quantile(Effective_Contacts,0.025),
            Q0.975=quantile(Effective_Contacts,0.975))




