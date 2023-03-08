library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Load in the sensitivity data
sd <- read_csv("datasets/sensitivity runs/SIRH_Sensitivity.csv")

# Convert to tidy data format
sd_tidy <- sd %>%
             pivot_longer(cols=!Days,names_to="Variable",values_to="Value") %>%
             mutate(Variable=str_replace_all(Variable,'\"',""),
                    Variable=str_replace(Variable,"=","")) %>%
             separate(Variable,into=c("Run","Variable"),sep = ":") %>%
             mutate(Variable=trimws(Variable)) %>%
             separate(Run,c("TempRun","Run"),sep = " ",convert = TRUE) %>%
             select(Run,Days,Variable,Value)

ggplot(filter(sd_tidy,Variable=="In Hospital"),
       aes(x=Days,y=Value,colour=Run,group=Run))+geom_line()

# widen the data

sd_wide <- sd_tidy %>%
             pivot_wider(names_from = Variable,values_from = Value)

# Nest the runs
sd_nest <- sd_wide %>%
             group_by(Run) %>%
             nest()

