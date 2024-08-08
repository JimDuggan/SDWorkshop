library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)

garments <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{

    Adjustment <- (Desired_Garments_in_Circulation-Garments_in_Circulation) /
      Garment_Adjustment_Time
    Garments_Produced     <- max(0,Adjustment+Expected_Garments_Discards)
    
    Garments_Discarded    <- Garments_in_Circulation *
                               Garment_Discard_Rate
    Increase_in_Discards   <- Garments_Discarded
    
    if(time >= Start_Time)
      Start_Flag <-  TRUE
    else
      Start_Flag <- FALSE
    
    if (Start_Flag)
        Adjustment_Fraction_Net <- Adjustment_Fraction
    else
        Adjustment_Fraction_Net <- 0
    
    Change_In_Discard_Rate <- (Desired_Garment_Discard_Rate-Garment_Discard_Rate) *
                               Adjustment_Fraction_Net
    
    Error_Term <- Garments_Discarded - Expected_Garments_Discards
    
    Change_In_EGD          <- Error_Term * Smoothing_Constant
    
    d_GIC_dt <- Garments_Produced - Garments_Discarded
    d_TD_dt  <- Garments_Discarded
    d_GDR_dt <- Change_In_Discard_Rate
    d_EGD_dt <- Change_In_EGD
    
    
    
    return (list(c(d_GIC_dt,d_TD_dt,d_GDR_dt,d_EGD_dt), 
                 F1_GP=Garments_Produced, 
                 F2_GD=Garments_Discarded,
                 F3_CEGD=Change_In_EGD,
                 F4_IID=Increase_in_Discards,
                 F5_CDR=Change_In_Discard_Rate,
                 A1_GAT=Garment_Adjustment_Time,
                 A2_DGIC=Desired_Garments_in_Circulation,
                 A3_ST=Start_Time,
                 P1_DGDD=Desired_Garment_Discard_Rate,
                 P2_AF=Adjustment_Fraction))
  })
}

run_scenario <- function(run_id=1,
                         Init_Garments_in_Circulation=100000,
                         Init_Discard_Rate=0.9,
                         Smoothing_Constant=0.5,
                         Start_Time=10,
                         Adjustment_Fraction=0.5,
                         Desired_Garment_Discard_Rate=0.5,
                         Desired_Garments_in_Circulation=100000,
                         Garment_Adjustment_Time=3,
                         simtime=seq(0,100,by=0.25)
){
  auxs <- c(Smoothing_Constant=Smoothing_Constant,
            Start_Time=Start_Time,
            Adjustment_Fraction=Adjustment_Fraction,
            Desired_Garment_Discard_Rate=Desired_Garment_Discard_Rate,
            Desired_Garments_in_Circulation=Desired_Garments_in_Circulation,
            Garment_Adjustment_Time=Garment_Adjustment_Time)
  
  
  stocks <- c(Garments_in_Circulation=Init_Garments_in_Circulation,
              Total_Discards=0,
              Garment_Discard_Rate=Init_Discard_Rate,
              Expected_Garments_Discards=Init_Garments_in_Circulation*
                                         Init_Discard_Rate)
  res <- ode(y=stocks, 
             times=simtime, 
             func = garments, 
             parms=auxs, 
             method="euler") 
  
  res <- res %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(RunID=as.integer(run_id)) %>%
    dplyr::select(RunID, everything())
}


