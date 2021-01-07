# TODO: consistent parameter naming strategy

PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

### Functions file to be read by mrgsolve / plotting / markdown files. 
library(tidyverse)
library(ggplot2)
library(stringr)
library(mrgsolve)
library(EnvStats)

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

get_conc_combi <- function(dat,which_run){
  
  #dat <- get_conc3(dat)
  
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

    StudyDose <- Study %>%
      filter(str_detect(NAME, "Dose") & str_detect(NAME,which_compound))%>%
      select(ID,NT,AMT)
    
    return(StudyDose)
}

# Pull and trim the data for use in mrgsolve*
# Should really just remove the which_run bit.. 

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

# Export concs from Study 3, combination runs. 
export_conc_combi <- function(Study,which_compound){
  
  
  conc_data <- get_conc_combi(get_conc3(Study),"Combination")%>%
    filter(Compound==which_compound)%>%
    select(ID,NT,DV)
  
  
  
  return(conc_data)
  
}

export_conc_combi_doses <- function(Study,which_compound){
  
  
  conc_data <- get_conc_combi(get_conc3(Study),"Combination")%>%
    filter(Compound==which_compound)%>%
    select(ID,NT,DV,Dose)
  
  
  
  return(conc_data)
  
}
export_conc_mono_doses <- function(studyDat,which_run){
  
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
      select(ID,NT,DV,Dose)
    
    
  }
  
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



# Optimize functions for mrgsolve: 

# Ordinary least sqaures # Function from Kyle Baron / metrum: https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md 
objOLS <- function(p, theta, data, dv ="conc", pred = FALSE) {
  
  names(p) <- names(theta)
  
  p <- lapply(p,exp)
  
  mod <- param(mod, p)
  
  out <- mod %>% param(p) %>% mrgsim_d(data, output="df")
  
  if(pred) return(out)
  
  sqr <- (out[["CP"]] - data[[dv]])^2
  
  sum(sqr, na.rm=TRUE)
}

# Least weighted sqaures # Function from Kyle Baron / metrum: https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md

objLWS <- function(p, theta, data, wt, pred = FALSE) {
  names(p) <- names(theta)
  p <- lapply(p,exp)
  out <- mod %>% param(p) %>% mrgsim_q(data, output="df")
  if(pred) return(out)
  return(sum(((out$CP - data[["conc"]])*wt)^2, na.rm=TRUE))
}


# Code draws heavily from metrum github resources:
# https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md

# DATA needs to be output of get_mrg functions, won't work otherwise..! 
mrg_model<-function(data, compartments, optimizer,output){
  
  print("data must be the output of get_mrgdata 12 or 3. Errors may be related to incorrect
        data entry.")
  print("Mod not found? Possibly run mod<-modlib(x) outside of the function. Seems to have issues booting
         within the loop for the first time in a session.")
  
  if(compartments==1){
    mod <- modlib("pk1")
    theta <- log(c(CL = 100, V = 80, KA = 10))
    thetaname <- c("CL","V","KA")
  }
  
  if(compartments==2){
    mod <- modlib("pk2")
    theta <- log(c(CL = 100, Q = 100, V2 = 80, V3 = 80, KA = 10))
    thetaname <- c("CL","Q","V2","V3","KA")
  }
  if(compartments==3){
    mod <- modlib("pk3cmt")
    theta <- log(c(CL = 100, Q = 100, VC = 80, VP = 80, VP2=80, KA1 = 10,Q2=100))
    thetaname <- c("CL","Q","VC","VP","VP2","KA1","Q2")
  }
  all_ID <- unique(data$ID)
  loop_ID <- 0
  
  return_param <- matrix(nrow=1,ncol=length(theta))
  return_pred <- list()
  
  # try creating a single row d.f.. 
  test_df <- data[1,] %>%
    mutate(mrgpred=NA)
  
  for( i in all_ID){
    
    loop_ID <- loop_ID+1
    subset_data <- filter(data,ID==i)
    
    if(optimizer=="OLS"){
      fit <- optim(par = theta, fn=objOLS, theta = theta, data=subset_data, method="Nelder-Mead")
      pred <- objOLS(fit$par, theta, subset_data, pred = TRUE)
    }
    
    if(optimizer=="OLS_min"){
      fit <- optim(par = theta, fn=objOLS, theta = theta, data=subset_data, method="L-BFGS-B",lower=0.01)
      pred <- objOLS(fit$par, theta, subset_data, pred = TRUE)
    }
    
    if(optimizer=="LWS"){
      dv <- subset_data[["conc"]]
      # add weighting factor.. 
      fit <- minqa::newuoa(par = theta, fn=objLWS, theta = theta, data=subset_data, wt=1/dv)
      pred <- objLWS(fit$par, theta, subset_data, wt = 1/dv, pred = TRUE)
    }
    
    return_param <- rbind(return_param,fit$par)
    
    #return_pred[[loop_ID]] <- pred$CP
    
    pred_df <- subset_data%>%
      mutate(mrgpred=pred$CP)
    
    test_df<-rbind(test_df,pred_df)
  } # patient loop
  
  if(output=="params"){
    return_param <- return_param[-1, ]  
    colnames(return_param)<-thetaname
    return(return_param)
  }
  if(output=="pred"){
    #return(return_pred)
    test_df<-test_df[-1,]
    return(test_df)
  }
}



get_geo_mean <- function(conc_data){
  
  doses <- unique(conc_data$Dose)
  mean_df <- data.frame(Dose=NA,NT=NA,GeoMean=NA)
  # for each timepoint, get the geo.mean
  for(d in doses){
    
    conc_subset <- conc_data %>%
      filter(Dose==d)
    
    times <- unique(conc_subset$NT)
    
    for( t in times){
      
      to_get_Mean <- conc_subset%>%
        filter(NT==t)
      
      to_get_Mean_vector <- as.numeric(to_get_Mean$DV)
      
      geo_mean <- geoMean(to_get_Mean_vector)
      
      df_bind <- data.frame(Dose=d,NT=t,GeoMean=geo_mean)
      mean_df <- rbind(mean_df,df_bind)
      
      
    }
  }
  mean_df<-mean_df[-1,]
  return(mean_df)
}

# Add dose data to mrgsolve predictions using output of export_conc_mono_dose or export_conc_combi_dose 

add_dose_data <- function(pred, conc){
  
  conc_subset <- conc %>%
    select(ID,Dose)%>%
    distinct()
  
  pred_dose <- pred%>%
    mutate(Dose=NA)
  
  for(i in 1:nrow(pred)){
    
    predID <- pred_dose[i,'ID']
    
    concrow <- filter(conc_subset,ID==predID)
    
    pred_dose[i,'Dose'] <- concrow['Dose']
  }
  
  return(pred_dose)
}


doseResponse24 <- function(data,which_compound){
  
  data24 <- data %>%
    filter(TIME==24)
  
  if(which_compound=="CpdA"){
    doses <- unique(data24$DOSECpdA)
  }
  
  if(which_compound=="CpdB"){
    doses <- unique(data24$DOSECpdB)
  }
  if(which_compound=="Combination"){
    data24 <- data24%>%
      mutate(combined_dose=DOSECpdA+DOSECpdB)
    
    doses <- unique(data24$combined_dose)
  }
  
  individual_data <- data.frame(dose=NA,response=NA)
  # Loop through each dose for the values of parasitaemia
  
  response<-c()
  for(i in doses){
    
    if(which_compound=="CpdA"){
      dosesubset <- data24%>%
        filter(DOSECpdA==i)
    }
    
    if(which_compound=="CpdB"){
      dosesubset <- data24%>%
        filter(DOSECpdB==i)
    }
    if(which_compound=="Combination"){
      dosesubset <- data24%>%
        filter(combined_dose==i)
    }
    
    individual_data_bind <- data.frame(dose=rep(i,times=length(dosesubset$DV)),response=dosesubset$DV)
    dose_mean<- mean(dosesubset$DV)
    
    response<-c(response,dose_mean)
    
    
    individual_data <- rbind(individual_data,individual_data_bind)
  }
  
  
  dose_response <- data.frame(Dose=doses,Response=response) %>%
    arrange(Dose)
  
  # Return dose_response if you want the mean.. 
  #return(dose_response)
  individual_data<-individual_data[-1,]
  return(individual_data)
}  



conc_response <- function(study,which_compound,combination){
  
  # Response is pulled by getpar. 
  
  
  # For the same study and compound, pull conc.. 
  if(combination==0){
    conc <- export_conc_mono(study,which_compound)
    response <- get_par(study,which_compound)
  }
  
  if(combination==1){
    conc <- export_conc_combi(study,which_compound)
    response <- get_par(study,"Combination")
  }
  
  trim_response <- response %>%
    select(ID,NT,DV)
  
  # conc is "pre-trimmed". 
  IDs <- unique(conc$ID)
  
  
  study_response_conc <- data.frame(TIME=NA,ID=NA,conc=NA,response=NA)
  for(i in IDs){
    
    conc_subset <- filter(conc,ID==i)
    response_subset <- filter(trim_response, ID==i)
    
    # Union or intersect? Either way... 
    # Also alters... hrm. 
    
    # MODIFIES 23 TO 24... BAD WORKAROUND
    conc_subset$NT[which(conc_subset$NT==23.0)]<-24
    
    time_intersect <- intersect(conc_subset$NT,response_subset$NT)
    
    # For each portion of time intersect, we store data at that time for conc and response for the ID.. 
    for(t in time_intersect){
      
      intersect_data <- data.frame(TIME=t, ID=i, conc=filter(conc_subset,NT==t)$DV,response=filter(response_subset,NT==t)$DV)
      
      study_response_conc <- rbind(study_response_conc,intersect_data)
    }
  }
  study_response_conc <- study_response_conc[-1,]
  return(study_response_conc)
}