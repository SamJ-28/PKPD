# mrgsolve PK fit try 2

# https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md

# To the reader: Most of this script is me experimenting with using mrgsolve to optimize parameter 
# fit as described in the above link. It's messy! This script was a "working script" to help 
# me write functions that use mrgsolve to estimate PK parameters / predictions from the provided data

# TODO: link to final functions 

library(mrgsolve)
library (dplyr)
library (nloptr)
library (ggplot2)

# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

# Read in drug concs:
Study1 <- get_individual_study(PKPDdata,"Study1")
# Drug conc 
Study1_CpdA_Conc <- export_conc_mono(Study1,"CpdA")
# Read in drug dose
Study1_dose <- get_dose_mono(Study1,"CpdA")

# Format data into a way that can be used by mrgsolve 
# i.e .   time conc evid cmt ID amt
Study1ID <- c(unique(Study1_CpdA_Conc$ID))

colnames(Study1_dose) <- c("ID","time","amt")
colnames(Study1_CpdA_Conc) <-c("ID","time","conc")

mrg_data <- data.frame(time=as.numeric(),conc=as.numeric(),evid=as.numeric(),cmt=as.numeric(),
                       ID=as.numeric(),amt=as.numeric())

# Loop to get data into a good format for mrgsolve. 
for(i in Study1ID){
  # add dose to mrg_data
  dose_subset <- Study1_dose %>%
    filter(ID==i) %>%
    mutate(evid=1,cmt=1,conc=NA) %>%
    relocate(time,conc,evid,cmt,ID,amt)
  
  
  # Now order and mutate conc, then rbind each ID, order by time.. 
  conc_subset <- Study1_CpdA_Conc %>%
    filter(ID==i) %>%
    mutate(evid=0,cmt=0,amt=0) %>%
    relocate(time,conc,evid,cmt,ID,amt)
  
  ID_subset <- rbind(dose_subset,conc_subset) %>%
    arrange(time,-evid)
  
  mrg_data <- rbind(mrg_data,ID_subset)
  
  
}

mrg_data9 <- filter(mrg_data,ID==9)

mrg_data22 <- filter(mrg_data,ID==22)
# Import obj function from kyle baron github:

obj <- function(p, theta, data, dv ="conc", pred = FALSE) {
  
  names(p) <- names(theta)
  
  p <- lapply(p,exp)
  
  mod <- param(mod, p)
  
  out <- mod %>% param(p) %>% mrgsim_d(data, output="df")
  
  if(pred) return(out)
  
  sqr <- (out[["CP"]] - data[[dv]])^2
  
  sum(sqr, na.rm=TRUE)
}

# least weighted sqaures
objlws <- function(p, theta, data, wt, pred = FALSE) {
  names(p) <- names(theta)
  p <- lapply(p,exp)
  out <- mod %>% param(p) %>% mrgsim_q(data, output="df")
  if(pred) return(out)
  return(sum(((out$CP - data[["conc"]])*wt)^2, na.rm=TRUE))
}

mod <- modlib("pk1")

param(mod)

theta <- log(c(CL = 100, V = 80, KA1 = 10))

#obj(theta,theta,mrg_data)

fit <- optim(par = theta, fn=obj, theta = theta, data=mrg_data9)

pred <- obj(fit$par, theta, mrg_data22, pred = TRUE)

mrg_data22$pred <- pred$CP


cmt_1_22 <-ggplot(data = mrg_data22) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(aes(time,pred),col="firebrick", lwd=1)


# Test new obj: objwls
dv <- mrg_data9[["conc"]]

fit_wt <- minqa::newuoa(par = theta, fn=objlws, theta = theta, data=mrg_data22, wt=1/dv)
objlws(fit_wt$par,theta,mrg_data22,dv)

pred <-  objlws(fit$par, theta, mrg_data22, wt = 1/dv, pred = TRUE)
predi <- objlws(theta,  theta, mrg_data22, wt = 1/dv, pred = TRUE)
predw <- objlws(fit_wt$par, theta, mrg_data22, wt = 1/dv, pred = TRUE) 


mrg_data22$pred <- pred$CP
mrg_data22$predi <- predi$CP



mrg_data22$predw <- predw$CP
head(mrg_data22)

ggplot(data = mrg_data22) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(data=mrg_data22,aes(time,pred),col="black", lwd=1, alpha = 0.6) +
  geom_line(data=mrg_data22,aes(time,predi),col="darkgreen", lwd=1) + 
  geom_line(data = mrg_data22, aes(time,predw), col="firebrick", lwd = 1)

# todo: functionalize, 
# individual runs

# 2cmt

mod <- modlib("pk2")
param(mod)

theta <- log(c(CL = 2, V2 = 50, Q = 10, V3 = 50))

obj <- function(p, theta, data, wt, pred = FALSE) {
  names(p) <- names(theta)
  p <- lapply(p,exp)
  out <- mod %>% param(p) %>% mrgsim_q(data, output="df")
  if(pred) return(out)
  return(sum(((out$CP - data[["conc"]])*wt)^2, na.rm=TRUE))
}

dv <- mrg_data22[["conc"]]

fit_wt <- minqa::newuoa(par = theta, fn=obj, theta = theta, data=mrg_data22, wt=1/dv)

pred <-  obj(fit$par, theta, mrg_data22, wt = 1/dv, pred = TRUE)
predi <- obj(theta,  theta, mrg_data22, wt = 1/dv, pred = TRUE)
predw <- obj(fit_wt$par, theta, mrg_data22, wt = 1/dv, pred = TRUE) 


mrg_data22$pred <- pred$CP
mrg_data22$predi <- predi$CP
mrg_data22$predw <- predw$CP
head(mrg_data22)

ggplot(data = mrg_data22) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(data=mrg_data22,aes(time,pred),col="black", lwd=1, alpha = 0.6) +
  geom_line(data=mrg_data22,aes(time,predi),col="darkgreen", lwd=1) + 
  geom_line(data = mrg_data22, aes(time,predw), col="firebrick", lwd = 1)


library(DEoptim)
library(tidyverse)

fit <- DEoptim::DEoptim(
  obj, 
  lower = rep(-4,4), 
  upper = rep(4,4), 
  theta = theta, data = mrg_data22, wt = 1/dv, 
  control = DEoptim.control(itermax=120,trace=20)
)


### 
# So, need a function to take outputs of export_conc and get_dose, run through mrgsolve 
# and return predictions and parameter values. 
# I think presenting the results of lws and newuoa should be enough - optimization not 
# my strong point anyway. 

# Takes study as character. Easier to have two functions - seperate one for study 3.
# This is not good programming, I would rather have one function to do everything 
# But study 3 is fiddly as it has both monotherapy runs and combination runs and i'm limited 
# on time. 

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