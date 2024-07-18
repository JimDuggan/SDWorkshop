library(deSolve)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggpubr)



sir <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    N     <- 10000        # Eq (11)
    IR    <- beta*I*S/N   # Eq (9) 
    RR    <- gamma*I      # Eq (10)
    dS_dt <- -IR          # Eq (6)
    dI_dt <- IR - RR      # Eq (7)
    dR_dt <- RR           # Eq (8)  
    return (list(c(dS_dt,dI_dt,dR_dt), 
                 Beta=beta, 
                 Gamma=gamma,
                 Infections=IR,
                 Recovering=RR))
  })
}


simtime <- seq(0,50,by=0.25)
stocks  <- c(S=9999,I=1,R=0)    # Eq (14), Eq (15), and Eq (16)
auxs    <- c(gamma=0.25,beta=1) # Eq (12) and Eq (13)

res <- ode(y=stocks, 
           times=simtime, 
           func = sir, 
           parms=auxs, 
           method="euler") %>%
  data.frame() %>%
  dplyr::as_tibble()
res



flows_piv <- res %>%
  dplyr::select(time,Infections:Recovering) %>%
  tidyr::pivot_longer(names_to="Flow",
                      values_to = "Value",
                      -time) %>%
  dplyr::mutate(Flow=factor(Flow,
                            levels = c("Infections",
                                       "Recovering")))

stocks_piv <- res %>%
  dplyr::select(time,S:R) %>%
  tidyr::pivot_longer(names_to="Stock",
                      values_to = "Value",
                      -time) %>%
  dplyr::mutate(Stock=factor(Stock,
                             levels = c("S","I","R")))


p1 <- ggplot(flows_piv,aes(x=time,y=Value,color=Flow))+geom_line()+
  theme(legend.position = "top")+
  labs(x="Day",y="Flows")

p2 <- ggplot(stocks_piv,aes(x=time,y=Value,color=Stock))+geom_line()+
  theme(legend.position = "top")+
  labs(x="Day",y="Stocks")

g1 <- ggarrange(p1,p2,nrow = 2)
g1


