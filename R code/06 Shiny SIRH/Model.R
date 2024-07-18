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

# sim <- run_sirh()
