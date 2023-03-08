library(aimsir17)
library(dplyr)
library(ggplot2)

summary(observations)

storm <- observations %>%
           filter(day %in% 15:17, month==10) %>%
           filter(station %in% c("MACE HEAD", "ROCHES POINT", "DUBLIN AIRPORT"))

ggplot(storm,aes(x=date,y=wdsp,colour=station))+
  geom_point()+geom_line()


ggplot(storm,aes(x=date,y=msl,colour=station))+
  geom_point()+geom_line()


ggplot(storm,aes(x=date,y=rain,colour=station))+
  geom_point()+geom_line()