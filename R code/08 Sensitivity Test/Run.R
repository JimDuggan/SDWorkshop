library(readsdr)
library(gt)

model_path <- "R code/08 Sensitivity Test/SIRH.stmx"
mdl        <- read_xmile(model_path)

sd_stocks(mdl) |> gt()

sd_constants(mdl) |> gt()

c_df <- data.frame(Infectivity= c(0,0.1,0.2,0.3,0.4))

sens_run <- sd_sensitivity_run(mdl$deSolve_components,
                               consts_df = c_df,
                               # stocks_df = s_df,
                               start_time = 0, 
                               stop_time = 100,
                               integ_method = "euler",
                               timestep     = 1/128)


