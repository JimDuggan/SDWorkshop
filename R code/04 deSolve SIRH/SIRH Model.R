# This R script implements the SIRH model and also performs
# a sensitivity sweep

library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)

sirh <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    IR  <- contacts * S * I/N * i
    RR  <- I * (1-HF) /D
    EH  <- I * HF / D
    RRH <- H / ALOS
    VR  <- S * VF
    
    dS_dt <- -IR - VR
    dI_dt <- IR - RR - EH
    dR_dt <- RR + RRH + VR
    dH_dt <- EH - RRH
    
    CheckSum <- S+I+R+H
    
    # browser()
  
    return (list(c(dS_dt,dI_dt,dR_dt,dH_dt),
                 IR=IR,RR=RR,EH=EH,RRH=RRH,Contacts=contacts,
                 Infectivity=i,ALOS=ALOS,HospFrac=HF,VaccFrac=VF,
                 CheckSum=CheckSum,N=N))   
  })
}



# It's useful to wrap the call to ode with a function
run_sirh <- function(start=0, 
                     finish=100,
                     step=0.125,
                     contacts=10,
                     infectivity=0.1,
                     duration=3,
                     N=100000,
                     ALOS=14,
                     HF=0.01,
                     inits=c(N-10,10,0,0),
                     VF=0.0){
  
  simtime <- seq(start, finish, step)
  # initialise vector of stocks
  stocks  <- c(S=inits[1],
               I=inits[2],
               R=inits[3],
               H=inits[4])
  
  # initialise vector of auxiliaries
  auxs    <- c(contacts=contacts,  # Contacts
               i=infectivity,      # Infectivity
               D=duration,         # Duration of infectiousness
               N=N,                # Total Population
               ALOS=ALOS,          # Average length of stay
               HF=HF,              # Hospitalisation Fraction
               VF=VF)              # VaccinationFraction
  
  
  sim <-data.frame(ode(y=stocks, 
                       times  = simtime, 
                       func   = sirh, 
                       parms  = auxs, 
                       method = "euler"))
  
  as_tibble(sim)
}

# One single run, default values
sim <- run_sirh()

p1 <- ggplot(sim,aes(x=time,y=H)) + 
        geom_point()+
        geom_line()

# Sensitivity sweep, modify 2 params
NSIMS <- 100
s_contacts <- sample(3:20,NSIMS,replace = T)
s_vacc     <- runif(NSIMS,min=0,max = 0.10)

count <- 1
# map2 is an iterator over two vectors
sens <- map2(s_contacts,s_vacc,~{
  message(glue("Sim {count} contacts {.x} vacc Fr {.y}"))
  out_sim <- run_sirh(contacts = .x,VF = .y) %>%
             mutate(Run=count) %>%
             select(Run,everything())
  count <<- count+1
  out_sim
})

full_sims <- bind_rows(sens)

p2 <- ggplot(full_sims,aes(x=time,y=H,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))


summ <- full_sims %>%
  group_by(Run) %>%
  summarise(PeakH=max(H),
            Contacts=first(Contacts),
            VF=first(VaccFrac)) %>%
  ungroup()


arrange(summ,desc(PeakH))

# Plot the results
p3 <- ggplot(summ,aes(x=Contacts,y=VF,size=PeakH,colour=PeakH))+
  geom_point()+
  scale_color_gradient(low="blue", high="red")+geom_jitter()




