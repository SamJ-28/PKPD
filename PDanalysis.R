# Script to investigate PD parameters 

library(tidyverse)
library(ggplot2)


# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

# Use get par to pull in parasitaemia data, which is % of parasitized eryhtrocytes 
# Pull in studies now.. 
study1 <- get_individual_study(PKPDdata,"Study1")
study2 <- get_individual_study(PKPDdata,"Study2")
study3 <- get_individual_study(PKPDdata,"Study3")

study1CpdA_par <- get_par(study1,"CpdA")
study2CpdB_par <- get_par(study2,"CpdB")


study1CpdA_par <- get_par(study1,"CpdA")
study2CpdB_par <- get_par(study2,"CpdB")


study3CpdA_par <- get_par(study3,"CpdA")
study3CpdB_par <- get_par(study3,"CpdB")
study3Combi_par <- get_par(study3,"Combination")

# pull in plot function 

spaghetti_par <- function(Study){
  
  print("Takes get_par")
  gg_conc <- ggplot()+
    geom_point(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    theme_bw()
  
  return(gg_conc)
}

# Facets are a nice way of splitting the very dense Study3 data.. 

spaghetti_par(study3Combi_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")

spaghetti_par(study1CpdA_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")

spaghetti_par(study3CpdA_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")


spaghetti_par(study3CpdB_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")
  
doseResponse48 <- function(data,which_compound){
  
  data48 <- data %>%
    filter(TIME==48)
  
  if(which_compound=="CpdA"){
   doses <- unique(data48$DOSECpdA)
  }
  
  if(which_compound=="CpdB"){
    doses <- unique(data48$DOSECpdB)
  }
  if(which_compound=="Combination"){
    data48 <- data48%>%
      mutate(combined_dose=DOSECpdA+DOSECpdB)
    
    doses <- unique(data48$combined_dose)
  }
  
  # Loop through each dose for the values of parasitaemia
  
  response<-c()
  for(i in doses){
    
    if(which_compound=="CpdA"){
      dosesubset <- data48%>%
        filter(DOSECpdA==i)
    }
    
    if(which_compound=="CpdB"){
      dosesubset <- data48%>%
        filter(DOSECpdB==i)
    }
    if(which_compound=="Combination"){
      dosesubset <- data48%>%
        filter(combined_dose==i)
    }
    
    dose_mean<- median(dosesubset$DV)
    
    response<-c(response,dose_mean)
  }
  
  dose_response <- data.frame(Dose=doses,Response=response) %>%
    arrange(Dose)
  
  return(dose_response)
}  

monoA <- doseResponse48(study3CpdA_par,"CpdA")
actualA <- doseResponse48(study1CpdA_par,"CpdA")

combiA<-doseResponse48(study3Combi_par,"CpdA")

monoB <- doseResponse48(study3CpdB_par,"CpdB")
actualB <- doseResponse48(study2CpdB_par,"CpdB")

combiB<-doseResponse48(study3Combi_par,"CpdB")

combi_add <- doseResponse48(study3Combi_par,"Combination")

dose_response <- ggplot()+
  geom_line(data=combi_add,aes(y=Response,x=Dose,color="blue"))+
  geom_line(data=monoA,aes(y=Response,x=Dose,color="Red"))+
  geom_line(data=monoB,aes(y=Response,x=Dose,color="Green"))
  