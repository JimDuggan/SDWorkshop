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


simtime <- seq(0,100,by=0.25)
stocks  <- c(P=100)            # Eq (5)
auxs    <- c(r=0.15,K=100000)  # Eq (3) and Eq (4)

res <- ode(y=stocks, 
           times=simtime, 
           func = ltg, 
           parms=auxs, 
           method="euler") %>%
  data.frame() %>%
  dplyr::as_tibble()
res

res_long <- res %>%
  dplyr::select(time,C,P,Flow) %>%
  tidyr::pivot_longer(names_to = "Variable", 
                      values_to = "Value",
                      -time)

ggplot(res_long,aes(x=time,y=Value,color=Variable)) + 
  geom_line() + facet_wrap(~Variable,scales = "free")+
  theme(legend.position = "top")

