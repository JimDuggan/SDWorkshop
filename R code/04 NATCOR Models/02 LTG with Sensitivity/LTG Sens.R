library(deSolve)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggpubr)

ltg <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    C <- P/K             # Eq (2)
    dP_dt <- r*P*(1-C)   # Eq (1)
    return (list(c(dP_dt), 
                 r=r, 
                 K=K,
                 C=C,
                 Flow=dP_dt))
  })
}

# Create a new "wrapper" function to parameterise each run
run_scenario <- function(run_id=1,
                         P=100, 
                         simtime=seq(0,100,by=0.25),
                         r=0.15,
                         K=100000){
  
  auxs    <- c(r=r,K=K)
  stocks <- c(P=P)

  res <- ode(y=stocks, 
             times=simtime, 
             func = ltg, 
             parms=auxs, 
             method="euler") %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(RunID=as.integer(run_id)) %>%
    dplyr::select(RunID, everything())
}

r_vals <- seq(0,.20,length.out=21)
p_vals  <- seq(0,1000,length.out=21)
sim_inputs <- expand.grid(r_vals,p_vals)

run_id <- 1

sim_res <- map2(sim_inputs[,1],
                sim_inputs[,2],~{
                  res <- run_scenario(run_id  = run_id,
                                      r   = .x,
                                      P   = .y,
                                      simtime = seq(0,100,by=0.25))
                  run_id <<- run_id + 1
                  res
                }) %>% dplyr::bind_rows()
sim_res

sum_vals <- sim_res %>%
  dplyr::group_by(time) %>%
  dplyr::summarize(MeanP=mean(P),
                   MedianP=median(P),
                   Q75=quantile(P,0.75),
                   Q25=quantile(P,0.25))
sum_vals

ggplot(sim_res,aes(x=time,y=P,color=RunID,group=RunID))+
  geom_line()+
  scale_color_gradientn(colors=rainbow(14))+
  theme(legend.position = "none")+
  labs(title="Population")+
  theme(title = element_text(size=9))

ggplot(sum_vals,aes(x=time,y=MeanP))+geom_line()+
  geom_line(aes(y=MedianP),colour="red")+
  geom_ribbon(aes(x=time,ymin=Q25,ymax=Q75),
              alpha=0.4,fill="steelblue2")+
  labs(title="50% quantiles for population")+
  theme(title = element_text(size=9))




