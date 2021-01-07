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

# These are the "monotherapy" of CpdA in Study 3
study3CpdA_par <- get_par(study3,"CpdA")
# "Monotherapy" of CpdB in Study 3
study3CpdB_par <- get_par(study3,"CpdB")
# These are the combination runs 
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
# Present these as the graphical interpretation of parasitaemia data

spaghetti_par(study3Combi_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 3 (Combination therapy)")

spaghetti_par(study1CpdA_par)+
  facet_grid(DOSECpdA~., labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 1 (Compound A)")

spaghetti_par(study2CpdB_par)+
  facet_grid(DOSECpdB~., labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 2 (Compound B)")

###################
#====================
###################

spaghetti_par(study3CpdA_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))


spaghetti_par(study3CpdB_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")



# This might be tricky. I'm reluctant to fudge time-points. 
# Can we NA missings? 
# This function might not be useful. 
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

S3CombiA <- conc_response(study3,"CpdA",1)

S3CombiA1Dose <- filter(conc_response(study3,"CpdA",1),TIME==24)

S1A <- conc_response(study1,"CpdA",0)
S1Dose <- filter(S1A,TIME==24)


conc_response<-ggplot()+
  geom_smooth(data=S3CombiA1Dose,aes(y=response,x=conc),method="glm",se=FALSE)+
  coord_cartesian(ylim=c(-1,1))

conc_response<-ggplot()+
  geom_point(data=S3CombiA1Dose,aes(y=response,x=conc))

conc_response<-ggplot()+
  geom_smooth(data=S1Dose,aes(y=response,x=conc),method="glm",se=FALSE)+
  coord_cartesian(ylim=c(-1,1))

# Pull in dose to double check 
S1AmonoDose <- get_dose_mono(study1,"CpdA")
S3AmonoDose <- get_dose_combi(study3,"CpdA",1)

# TODO: Response is time 24... but conc isn't. 