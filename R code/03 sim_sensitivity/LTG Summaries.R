library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Load in the sensitivity data
sd <- read_csv("datasets/sensitivity runs/LTG_Sensitivity.csv")

# Convert to tidy data format
sd_tidy <- sd %>%
             pivot_longer(cols=!Year,names_to="Variable",values_to="Value") %>%
             mutate(Variable=str_replace_all(Variable,'\"',""),
                    Variable=str_replace(Variable,"=","")) %>%
             separate(Variable,into=c("Run","Variable"),sep = ":") %>%
             mutate(Variable=trimws(Variable)) %>%
             separate(Run,c("TempRun","Run"),sep = " ",convert = TRUE) %>%
             select(Run,Year,Variable,Value)

ggplot(filter(sd_tidy,Variable=="Population"),
       aes(x=Year,y=Value,group=Run))+geom_line()

sum_runs <- sd_tidy %>%
              filter(Variable=="Population") %>%
              group_by(Variable,Year) %>%
              summarise(Q75=quantile(Value,0.75),
                        Q25=quantile(Value,0.25),
                        Median=median(Value),
                        Mean=mean(Value))


ggplot(sum_runs,aes(Year, Median)) + 
  geom_ribbon(aes(ymin = Q25,ymax = Q75),fill = "steelblue2") + 
  geom_line(color = "firebrick")


            
