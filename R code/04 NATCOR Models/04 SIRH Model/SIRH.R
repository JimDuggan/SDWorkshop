sirh <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    N <- 10000               # Eq (31)
    IR <- beta*I*S/N*M       # Eq (22)
    VR <- v*S                # Eq (23)
    AR <- hf*gamma*I         # Eq (24)
    RR <- (1-hf)*gamma*I     # Eq (25)
    DR <- d*H                # Eq (26)
    
    dS_dt  <- -IR -VR        # Eq (17)
    dI_dt  <-  IR - AR - RR  # Eq (18)
    dH_dt  <-  AR - DR       # Eq (19)
    dR_dt  <-  RR + DR + VR  # Eq (20)
    dM_dt  <- (M_min - M) * 
      alpha       # Eq (21)
    return (list(c(dS_dt,dI_dt,dH_dt,dR_dt,dM_dt), 
                 Beta=beta, 
                 Gamma=gamma,
                 HF=hf,
                 V=v,
                 Alpha=alpha,
                 M_Min=M_min,
                 Infections=IR,
                 Recovering=RR,
                 Vaccinated=VR,
                 Hospitalised=AR,
                 Discharged=DR,
                 CheckSum=S + I + R + H))
  })
}

run_scenario <- function(run_id=1,
                         stocks=c(S=9999, # Eq (34)
                                  I=1,    # Eq (35)
                                  H=0,    # Eq (36)
                                  R=0,    # Eq (37)
                                  M=1     # Eq (38)
                         ), 
                         simtime=seq(0,50,by=0.25),
                         beta=1,       # Eq (27)
                         gamma=0.25,   # Eq (28)
                         alpha=0.5,    # Eq (23)
                         M_min=0.3,    # Eq (39)
                         d=0.1,        # Eq (32)
                         hf=0.1,       # Eq (30)
                         v=0.1         # Eq (29) 
){
  auxs <- c(beta=beta,
            gamma=gamma,
            alpha=alpha,
            M_min=M_min,
            d=d,
            hf=hf,
            v=v)
  
  res <- ode(y=stocks, 
             times=simtime, 
             func = sirh, 
             parms=auxs, 
             method="euler") %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(RunID=as.integer(run_id)) %>%
    dplyr::select(RunID, everything())
}


alpha_vals <- seq(0,.20,length.out=50)
vacc_vals  <- seq(0,0.05,length.out=50)
sim_inputs <- expand.grid(alpha_vals,vacc_vals)
summary(sim_inputs)

run_id <- 1

sim_res <- map2(sim_inputs[,1],
                sim_inputs[,2],~{
                  res <- run_scenario(run_id  = run_id,
                                      alpha   = .x,
                                      v       = .y,
                                      simtime = seq(0,75,by=0.25),
                                      hf      = 0.10)
                  run_id <<- run_id + 1
                  res
                }) %>% dplyr::bind_rows()
sim_res


time_h <- sim_res %>%
  dplyr::group_by(time) %>%
  dplyr::summarize(MeanH=mean(H),
                   Q95=quantile(H,0.95),
                   Q05=quantile(H,0.05))
time_h

p3 <- ggplot(sim_res,aes(x=time,y=Infections,color=RunID,group=RunID))+
  geom_line()+
  scale_color_gradientn(colors=rainbow(14))+
  theme(legend.position = "none")+
  labs(title="Infections (flow)")+
  theme(title = element_text(size=9))

p4 <- ggplot(sim_res,aes(x=time,y=H,color=RunID,group=RunID))+
  geom_line()+
  scale_color_gradientn(colors=rainbow(14))+
  theme(legend.position = "none")+
  labs(title="People in hospital (stock)")+
  theme(title = element_text(size=9))

p5 <- ggplot(time_h,aes(x=time,y=MeanH))+geom_line()+
  geom_ribbon(aes(x=time,ymin=Q05,ymax=Q95),
              alpha=0.4,fill="steelblue2")+
  labs(title="90% quantiles for people in hospital")+
  theme(title = element_text(size=9))

g2 <- ggarrange(p3,p4,p5,nrow = 3)
g2


max_h <- sim_res %>%
  dplyr::group_by(RunID) %>%
  dplyr::summarize(MH=max(H),
                   V=first(V),
                   Alpha=first(Alpha))


p6 <- ggplot(max_h,aes(x=Alpha,y=V,color=MH,size=MH))+geom_point()+
  scale_color_gradient(low="blue", high="red")+
  theme(legend.position = "none")+
  labs(title=paste0("Parameter Analysis"))+
  labs(subtitle=paste0("Max peak = ",
                       round(max(max_h$MH),0),
                       " at point (0,0)"))+
  theme(title = element_text(size=9))


p7 <- ggplot(max_h,aes(x=Alpha,y=V,z=MH))+geom_contour_filled()+
  theme(legend.position = "none")+
  labs(title=paste0("Contour plot"))+
  labs(subtitle=paste0("Yellow band range (450,500]"))+
  theme(title = element_text(size=9))

g3 <- ggarrange(p6,p7,nrow = 1)
g3



