### Functions file to be read by mrgsolve / plotting / markdown files. 
library(tidyverse)
library(ggplot2)
library(stringr)

# Isolates Study 1, 2, 3 from the full PKPData (which should be hard-read in)

get_individual_study <- function(data,study){
  print("Input should be Study1,Study2 etc. Relies on data being correctly labelled in input, careful.")
  
  individual_study <- data[grep(study,data$USUBJID),]
  return(individual_study)
}

# Gets conc for the monotherapy studies (1, 2) and feeds subsequent function for Study 3
# Takes the output of get_individual_study

get_conc3 <- function(studyDat){
  
  # StudyDat is output of get_individual_study
  
  StudyDose <- studyDat[grep("Concentration",studyDat$NAME),]
  
  CpdA<- StudyDose %>%
    filter(str_detect(NAME, "CpdA")) %>%
    mutate(Compound="CpdA",Dose=DOSECpdA)
  
  CpdB <- StudyDose %>%
    filter(str_detect(NAME, "CpdB")) %>%
    mutate(Compound="CpdB",Dose=DOSECpdB)
  
  editedStudyDose <- rbind(CpdA,CpdB) %>%
    filter(!str_detect(IGNORE,"LLOQ"))
  
  
  return(editedStudyDose)
}

# Takes input of get_conc3(Study3), will work with Study 1 and 2 (but not needed)
get_conc_combi <- function(dat,which_run){
  
  print("Takes the input of get_conc3(study)")
  if(which_run=="CpdA"){
    dat_test <- dat %>%
      filter(str_detect(TRTNAME, "CpdA") & !str_detect(TRTNAME,"CpdB"))
  }
  
  if(which_run=="CpdB"){
    dat_test <- dat %>%
      filter(str_detect(TRTNAME, "CpdB") & !str_detect(TRTNAME,"CpdA"))
  }
  
  if(which_run=="Combination"){
    dat_test <- dat %>%
      filter(str_detect(TRTNAME, "CpdB") & str_detect(TRTNAME,"CpdA"))
  }
  
  return(dat_test)  
}