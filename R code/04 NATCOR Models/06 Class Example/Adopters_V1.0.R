adopters <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    N <- Potential_Adopters + Adopters
    
    AR <- Effective_Contacts*
          Adopters*
          Potential_Adopters/N
    
    dPA_dt <- -AR
    dA_dt  <-  AR

    return (list(c(dPA_dt,dA_dt), 
                 Effective_Contacts=Effective_Contacts, 
                 N=N))
  })
}

run_scenario <- function(run_id=1,
                         stocks=c(Potential_Adopters=9999,
                                  Adopters=1), 
                         simtime=seq(0,50,by=0.25),
                         Effective_Contacts=1){
  auxs <- c(Effective_Contacts=Effective_Contacts)
  
  res <- ode(y=stocks, 
             times=simtime, 
             func = adopters, 
             parms=auxs, 
             method="euler") %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(RunID=as.integer(run_id)) %>%
    dplyr::select(RunID, everything())
}
