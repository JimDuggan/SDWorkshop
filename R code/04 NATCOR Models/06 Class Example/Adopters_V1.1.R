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
                         simtime=seq(0,5,by=0.25),
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

adopter_vals <- seq(0,1000,length.out=51)
eff_con_vals  <- seq(0,1,length.out=51)
sim_inputs <- expand.grid(adopter_vals,eff_con_vals)
summary(sim_inputs)

run_id <- 1

sim_res <- map2(sim_inputs[,1],
                sim_inputs[,2],~{
                  res <- run_scenario(run_id  = run_id,
                                      stocks   = c(Potential_Adopters=10000-.x,
                                                   Adopters=.x),
                                      Effective_Contacts      = .y)
                  run_id <<- run_id + 1
                  res
                }) %>% dplyr::bind_rows()
sim_res

p1 <- ggplot(sim_res,aes(x=time,y=Adopters,color=RunID,group=RunID))+
  geom_line()+
  scale_color_gradientn(colors=rainbow(14))+
  theme(legend.position = "none")+
  labs(title="Adopters (Stock)")+
  theme(title = element_text(size=9))

max_a <- sim_res %>%
  dplyr::group_by(RunID) %>%
  dplyr::summarize(MA=max(Adopters),
                   A=first(Adopters),
                   EC=first(Effective_Contacts))

p2 <- ggplot(max_a,aes(x=EC,y=A,z=MA))+geom_contour_filled()+
  labs(title=paste0("Contour plot"))+
  theme(title = element_text(size=9))

p3 <- ggplot(max_a,aes(x=EC,y=A,color=MA,size=MA))+geom_point()+
  scale_color_gradient(low="blue", high="red")+
  labs(title=paste0("Parameter Analysis"))+
  labs(subtitle=paste0("Max A = ",
                       round(max(max_a$MA),0),
                       " at point (0,0)"))+
  theme(title = element_text(size=9))

