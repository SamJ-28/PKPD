# TODO: consistent parameter naming strategy

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

# Gets conc for study 3 and selects the monotherapies or the combination 
# Takes input of get_conc3(Study3), will work with Study 1 and 2 (but not needed)
# todo: make it just take study object i.e the output of get_individual_study

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

# Gets parasitaemia data
get_par <- function(studyDat, which_treatment){
  print("Takes Study1, Study2, etc..")
  
  Study_par <- studyDat %>%
    filter(str_detect(NAME, "Parasitemia")) 
  
  # Can we pipe these multiple if statements?
  if(which_treatment=="CpdA"){
    Study_par<- Study_par %>%
      filter(str_detect(TRTNAME, "CpdA") & !str_detect(TRTNAME,"CpdB"))
  }
  
  if(which_treatment=="CpdB"){
    Study_par<- Study_par %>%
      filter(str_detect(TRTNAME, "CpdB") & !str_detect(TRTNAME,"CpdA"))
    
  }
  
  if(which_treatment=="Combination"){
    Study_par<- Study_par %>%
      filter(str_detect(TRTNAME, "CpdB") & str_detect(TRTNAME,"CpdA"))
  }
  
  print(class(Study_par))
  
  return(data.frame(Study_par))
}

# Gets dose
# Study 3 having both combination and monotherapies makes these functions really clunky.. 
# In an ideal world they would be treated as seperate studies for data entry.
# There's some hard-coding in here for now due to time - want to get the mrgsolve model working. 

# todos: select

get_dose_combi <- function(Study,which_compound,combination){
  
  print("Only use for Study 3")
  if(combination==1){
  StudyDose <-Study %>%
    filter(str_detect(TRTNAME, "CpdB") & str_detect(TRTNAME,"CpdA") & str_detect(NAME,"Dose")) %>%
    filter(str_detect(NAME,which_compound))
  }
  
  if(combination==0){
    
    if(which_compound=="CpdA"){
    ignorecomp <- "CpdB"
    }
    
    if(which_compound=="CpdB"){
      ignorecomp <- "CpdA"
    }
    
    StudyDose <-Study %>%
      filter(str_detect(TRTNAME, which_compound) & !str_detect(TRTNAME, ignorecomp) & 
               str_detect(NAME,"Dose"))
  
  }
  
  # todo select
  
  StudyDose <- StudyDose%>%
    select(ID,NT,AMT)
  
  return(StudyDose)
}

# which compound is derelict here 
get_dose_mono <-function(Study,which_compound){
    print("Only use for Study 1 and 2")
    StudyDose <- Study %>%
      filter(str_detect(NAME, "Dose") & str_detect(NAME,which_compound))%>%
      select(ID,NT,AMT)
    
    return(StudyDose)
}

# Pull and trim the data for use in mrgsolve*
export_conc_mono <- function(studyDat,which_run){
  
  # which_run is only needed for study 3
  if(missing(which_run)){
    
    # load drug data
    conc_data <- get_conc3(studyDat) %>%
      select(ID,NT,DV)
    
  }
  # Study 3 needs to specify which compound
  # As it has 2x monotherapies and a combination therapy (that itself has 2x drug concs)
  else{
    
    print("don't use this function for getting the combination data, it needs to be split")    
    conc_data <- get_conc_combi(get_conc3(studyDat),which_run)%>%
      select(ID,NT,DV)
    
    
  }
  
  return(conc_data)
  
}

export_conc_combi <- function(Study,which_compound){
  
  
  conc_data <- get_conc_combi(get_conc3(Study),"Combination")%>%
    filter(Compound==which_compound)%>%
    select(ID,NT,DV)
  
  
  
  return(conc_data)
  
}

# Functions to port data into mrgsolve models

get_mrgdata_12<-function(Study,which_compound){
  
  # Call this once at the start, then split to conc/dose 
  study_data <- get_individual_study(PKPDdata,Study)
  
  # Drug conc 
  study_conc <- export_conc_mono(study_data,which_compound)
  # Read in drug dose
  study_dose <- get_dose_mono(study_data,which_compound)
  
  studyID <- c(unique(study_conc$ID))
  
  colnames(study_dose) <- c("ID","time","amt")
  colnames(study_conc) <-c("ID","time","conc")
  
  mrg_data <- data.frame(time=as.numeric(),conc=as.numeric(),evid=as.numeric(),cmt=as.numeric(),
                         ID=as.numeric(),amt=as.numeric())
  
  # Change the data to format usable by mrgsolve:
  for(i in studyID){
    # add dose to mrg_data
    dose_subset <- study_dose %>%
      filter(ID==i) %>%
      mutate(evid=1,cmt=1,conc=NA) %>%
      relocate(time,conc,evid,cmt,ID,amt)
    
    
    # Now order and mutate conc, then rbind each ID, order by time.. 
    conc_subset <- study_conc %>%
      filter(ID==i) %>%
      mutate(evid=0,cmt=0,amt=0) %>%
      relocate(time,conc,evid,cmt,ID,amt)
    
    ID_subset <- rbind(dose_subset,conc_subset) %>%
      arrange(time,-evid)
    
    mrg_data <- rbind(mrg_data,ID_subset)
    
    
  }
  
  return(mrg_data)
  
}


get_mrgdata_3<-function(Study,which_run,which_compound){
  
  
  if(which_run!="Combination"){
    # Call this once at the start, then split to conc/dose 
    study_data <- get_individual_study(PKPDdata,Study)
    
    study_conc <- export_conc_mono(study_data,which_compound)
    # Read in drug dose
    study_dose <- get_dose_mono(study_data,which_compound)
  }
  
  
  if(which_run=="Combination"){
    
    # Get study3
    study_data <- get_individual_study(PKPDdata,Study)
    
    
    study_conc <- export_conc_combi(study_data,which_compound)
    # Read in drug dose
    study_dose <- get_dose_combi(study_data,which_compound,1)
    
  }
  
  studyID <- c(unique(study_conc$ID))
  
  colnames(study_dose) <- c("ID","time","amt")
  colnames(study_conc) <-c("ID","time","conc")
  
  mrg_data <- data.frame(time=as.numeric(),conc=as.numeric(),evid=as.numeric(),cmt=as.numeric(),
                         ID=as.numeric(),amt=as.numeric())
  
  # Change the data to format usable by mrgsolve:
  for(i in studyID){
    # add dose to mrg_data
    dose_subset <- study_dose %>%
      filter(ID==i) %>%
      mutate(evid=1,cmt=1,conc=NA) %>%
      relocate(time,conc,evid,cmt,ID,amt)
    
    
    # Now order and mutate conc, then rbind each ID, order by time.. 
    conc_subset <- study_conc %>%
      filter(ID==i) %>%
      mutate(evid=0,cmt=0,amt=0) %>%
      relocate(time,conc,evid,cmt,ID,amt)
    
    ID_subset <- rbind(dose_subset,conc_subset) %>%
      arrange(time,-evid)
    
    mrg_data <- rbind(mrg_data,ID_subset)
    
    
  }
  
  return(mrg_data)
}