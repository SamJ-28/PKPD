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

# Study 3 : export_conc_mono for the monotherapy runs
Study3_MONO_CpdA_Conc <- export_conc_mono_doses(study3,"CpdA")
Study3_MONO_CpdB_Conc <- export_conc_mono_doses(study3,"CpdB")

# Combination 
Study3_COMBI_CpdA_Conc <- export_conc_combi_doses(study3,"CpdA")
Study3_COMBI_CpdB_Conc <- export_conc_combi_doses(study3,"CpdB")

# Base function for spaghetti plots.
spaghetti <- function(Study){

  gg_conc <- ggplot()+
    geom_point(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    theme_bw()
  
  return(gg_conc)
}
spaghetti_nPoint <- function(Study){
  
  gg_conc <- ggplot()+
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
 

#ggsave("spaghetti_study2_conc_dose.tiff")


spaghetti_study3_conc <-spaghetti(study3_conc)+
  ggtitle("Study 3, concentration of compound A and compound B")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Patient ID")+
  geom_vline(xintercept=dose_times,linetype="dashed")+
  facet_grid(.~Compound)

#ggsave("spaghetti_study3_conc.tiff")

study3_conc_CpdA <- filter(study3_conc, Compound=="CpdA")
study3_conc_CpdB <- filter(study3_conc, Compound=="CpdB")

spaghetti_study3_CpdA_conc <-spaghetti(study3_conc_CpdA)+
  ggtitle("Study 3, concentration of compound A")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Patient ID")+
  geom_vline(xintercept=dose_times,linetype="dashed")+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  theme(legend.position = "none",strip.text.y = element_text(angle = 0))

#ggsave("spaghetti_study3_CpdA_conc.tiff")

spaghetti_study3_CpdB_conc <-spaghetti(study3_conc_CpdB)+
  ggtitle("Study 3, concentration of compound B")+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  labs(color="Patient ID")+
  geom_vline(xintercept=dose_times,linetype="dashed")+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  theme(legend.position = "none",strip.text.y = element_text(angle = 0))

ggsave("spaghetti_study3_CpdB_conc.tiff")