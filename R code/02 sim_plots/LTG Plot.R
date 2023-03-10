library(ggplot2)
library(readr)
library(dplyr)

d <- read_csv("datasets/single runs/LTG.csv")

ggplot(d,aes(x=Year,y=Population)) + 
  geom_point()+geom_line()

d <- d %>%
  mutate(ModeValue=round(c(NA,NA,diff(abs(diff(Population)))),3),
         Mode=as.factor(ifelse(ModeValue>0,"EXP",
                     ifelse(ModeValue<0,"LOG","LIN"))))

ggplot(d,aes(x=Year,y=Population,colour=Mode)) + 
  geom_point()+theme_light()



ggplot(d,aes(x=Year,y=Population,colour=Mode)) + 
  geom_point()+geom_line()+theme_classic()

ggplot(d,aes(x=Year,y=Population,colour=Mode)) + 
  geom_point()+geom_line()+theme_classic()+facet_wrap(~Mode)

ggplot(d,aes(x=Additions,y=Population,colour=Mode))+geom_point()


