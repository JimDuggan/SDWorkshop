# This example takes as input a sensitivity run from Stella and then
# generates a scatter plot of two params (contacts and Vaccination Fraction)
# to show the peak values in hospital

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

# widen the data because we want to see each variable in its own column
sd_wide <- sd_tidy %>%
             pivot_wider(names_from = Variable,values_from = Value)


# Gather the peak data for each run
summ <- sd_wide %>%
           group_by(Run) %>%
           summarise(PeakH=max(`In Hospital`),
                     Contacts=first(Contacts),
                     VF=first(VF)) %>%
          ungroup()

# Show the tibble
arrange(summ,desc(PeakH))

# PLot the results
ggplot(summ,aes(x=Contacts,y=VF,size=PeakH,colour=PeakH))+
  geom_point()+
  scale_color_gradient(low="blue", high="red")+geom_jitter()

