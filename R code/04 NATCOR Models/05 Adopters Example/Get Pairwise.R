library(tidyverse)
library(ggpubr)
library(GGally)
library(readxl)
library(glue)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d(aes(fill=..density..), geom="tile", contour = FALSE) +
    scale_fill_viridis_c()
  p
}

cor_fun <- function(data, mapping, method="pearson", ndp=2, sz=5, stars=TRUE, ...){
  
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor.test(x, y, method=method)
  est <- corr$estimate
  lb.size <- sz* abs(est) 
  
  if(stars){
    stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
    lbl <- paste0(round(est, ndp), stars)
  }else{
    lbl <- round(est, ndp)
  }
  
  ggplot(data=data, mapping=mapping) + 
    annotate("text", x=mean(x, na.rm=TRUE), y=mean(y, na.rm=TRUE), label=lbl, size=5,...)+
    theme(panel.grid = element_blank())
}


get_pair_wise <- function(posterior,f_name){
  
  p1 <- ggpairs(posterior, lower=list(continuous=my_fn),
                diag=list(continuous="densityDiag"),
                upper=list(continuous=wrap(cor_fun, sz=5, stars=FALSE)),
                title=f_name
  )
}



