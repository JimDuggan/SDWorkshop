library(ggplot2)

summary(mpg)

ggplot(mpg,aes(x=displ,y=cty))+geom_point()+geom_smooth(method="lm")

ggplot(mpg,aes(x=displ,y=cty,colour=class))+geom_point()

