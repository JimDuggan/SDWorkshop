source("R code/07 SRBS/Garments.R")

library(ggplot2)
library(ggpubr)


range_Adjustment_Fraction          <- seq(0,1,length.out=50)
range_Desired_Garment_Discard_Rate <- seq(0,1,length.out=50)

sim_inputs <- expand.grid(range_Desired_Garment_Discard_Rate,
                          range_Adjustment_Fraction)
summary(sim_inputs)

run_id <- 1

sim_res <- map2(sim_inputs[,1],
                sim_inputs[,2],~{
                  res <- run_scenario(run_id  = run_id,
                                      Adjustment_Fraction = .x,
                                      Desired_Fashion_Obsolescence_Rate = .y)
                  run_id <<- run_id + 1
                  res
                }) %>% dplyr::bind_rows()
sim_res


time_h <- sim_res %>%
  dplyr::group_by(time) %>%
  dplyr::summarize(TotalDiscardsMean=mean(Total_Discards),
                   Q95=quantile(Total_Discards,0.95),
                   Q05=quantile(Total_Discards,0.05))
time_h

p3 <- ggplot(sim_res,aes(x=time,y=Total_Discards,color=RunID,group=RunID))+
  geom_line()+
  scale_color_gradientn(colors=rainbow(14))+
  theme(legend.position = "none")+
  labs(title="Infections (flow)")+
  theme(title = element_text(size=9))


p5 <- ggplot(time_h,aes(x=time,y=TotalDiscardsMean))+geom_line()+
  geom_ribbon(aes(x=time,ymin=Q05,ymax=Q95),
              alpha=0.4,fill="steelblue2")+
  labs(title="90% quantiles for people in hospital")+
  theme(title = element_text(size=9))

# g2 <- ggarrange(p3,p4,p5,nrow = 3)
# g2


max_TD <- sim_res %>%
  dplyr::group_by(RunID) %>%
  dplyr::summarize(MTD=max(Total_Discards),
                   AF=first(P2_AF),
                   DGDD=first(P1_DFOR))


p6 <- ggplot(dplyr::sample_n(max_TD,250),aes(x=AF,y=DGDD,color=MTD,size=MTD))+geom_point()+
  scale_color_gradient(low="blue", high="red")+
  ylab("Desired Fashion Obsolescence Rate")+
  xlab("Adjustment Fraction")+
  # labs(title=paste0("Sensitivity Analysis"))+
  theme(title = element_text(size=9))


p7 <- ggplot(max_TD,aes(x=AF,y=DGDD,z=MTD))+geom_contour_filled()+
  ylab("Desired Fashion Obsolescence Rate")+
  xlab("Adjustment Fraction")+
  theme(title = element_text(size=9))

g3 <- ggarrange(p6,p7,nrow = 1)
g3



