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
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 3 (Combination therapy)")

#ggsave("Study3_par.tiff")

spaghetti_par(study1CpdA_par)+
  facet_grid(DOSECpdA~., labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 1 (Compound A)")


#ggsave("Study1_par.tiff")

spaghetti_par(study2CpdB_par)+
  facet_grid(DOSECpdB~., labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 2 (Compound B)")


#ggsave("Study2_par.tiff")
###################
#====================
###################

# Conc response needs to be single dose i.e. that the response is taken at 23/24 
# Before the second dose.. 

S3CombiA <- conc_response(study3,"CpdA",1)
S3CombiA1Dose <- filter(S3CombiA,TIME==24)

S3CombiB <- conc_response(study3,"CpdB",1)
S3CombiB1Dose <- filter(S3CombiB,TIME==24)

S1A <- conc_response(study1,"CpdA",0)
S1Dose <- filter(S1A,TIME==24)

S2B <- conc_response(study2,"CpdB",0)
S2Dose <- filter(S2B,TIME==24)
##################
# Plots of conc response
##################

S3Aconc_response<-ggplot()+
  geom_point(data=S3CombiA1Dose,aes(y=response,x=log(conc)))+
  coord_cartesian(ylim=c(-1,1))

S3Bconc_response<-ggplot()+
  geom_point(data=S3CombiB1Dose,aes(y=response,x=conc))+
  coord_cartesian(ylim=c(-1,1))

S1conc_response<-ggplot()+
  geom_point(data=S1A,aes(y=response,x=conc))+
  coord_cartesian(ylim=c(-1,1))

S2conc_response<-ggplot()+
  geom_point(data=S2B,aes(y=response,x=conc))+
  coord_cartesian(ylim=c(-1,1))


conc_response<-ggplot()+
  geom_smooth(data=S1Dose,aes(y=response,x=conc),method="glm",se=FALSE)+
  coord_cartesian(ylim=c(-1,1))

# Pull in dose to double check 
S1AmonoDose <- get_dose_mono(study1,"CpdA")
S3AmonoDose <- get_dose_combi(study3,"CpdA",1)

# TODO: Response is time 24... but conc isn't. 