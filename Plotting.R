# Script to generate exploratory plots of data. 


library(tidyverse)
library(ggplot2)


# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")


study1 <- get_individual_study(PKPDdata,"Study1")
study2 <- get_individual_study(PKPDdata,"Study2")
study3 <- get_individual_study(PKPDdata,"Study3")

study1_conc <- export_conc_mono_doses(study1,"CpdA")
study2_conc <- export_conc_mono_doses(study2,"CpdB")
# Base function for spaghetti plots.
spaghetti <- function(Study){

  gg_conc <- ggplot()+
    geom_point(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    theme_bw()
  
  return(gg_conc)
}

# Colors by dose 
spaghetti_dose <- function(Study){
  
  gg_conc <- ggplot()+
    geom_point(data=Study,aes(y=DV,x=NT,color=as.factor(Study$Dose),group=as.factor(Study$ID)))+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$Dose),group=as.factor(Study$ID)))+
    theme_bw()
  
  return(gg_conc)
}

# Dose times 
dose_times <- c(0,24,48,72)

spaghetti_study1_conc <-spaghetti(study1_conc)+
  ggtitle("Study 1, concentration of compound A")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Patient ID")+
  geom_vline(xintercept=dose_times,linetype="dashed")

#ggsave("spaghetti_study1_conc.tiff")  


spaghetti_study1_dose <-spaghetti_dose(study1_conc)+
  ggtitle("Study 1, concentration of compound A")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Dose amount (mg/kg)")+
  scale_color_manual(breaks = c("1", "5", "10","25","50","75","100"),
                     values=c("red", "orange", "green","blue","brown","violet","black"))+
  geom_vline(xintercept=dose_times,linetype="dashed")

#ggsave("spaghetti_study1_conc_dose.tiff")


spaghetti_study2_conc <-spaghetti(study2_conc)+
  ggtitle("Study 2, concentration of compound B")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Patient ID")+
  geom_vline(xintercept=dose_times,linetype="dashed")


#ggsave("spaghetti_study2_conc.tiff") 

spaghetti_study2_dose <-spaghetti_dose(study2_conc)+
  ggtitle("Study 2, concentration of compound B")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Dose amount (mg/kg)")+
  geom_vline(xintercept=dose_times,linetype="dashed")
 

ggsave("spaghetti_study21_conc_dose.tiff")