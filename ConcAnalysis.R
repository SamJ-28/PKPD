# Drug conc comparison plotting

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

# Spaghetti plots in data_clean are fine for showing each individual as a "here's this data" sort of thing.. 

# But I want to fit conc / time lines by dose.
# Export_conc functions currently don't have Dose attatched - get_conc3 does.
# I don't want to mess with the export_conc functions as mrgsolve needs data in a certain format.
# Easier to just select down the data here. 

Study1_CpdA_Conc <- export_conc_mono_doses(study1,"CpdA")
Study2_CpdB_Conc <- export_conc_mono_doses(study2,"CpdB")

# Study 3 : export_conc_mono for the monotherapy runs
Study3_MONO_CpdA_Conc <- export_conc_mono_doses(study3,"CpdA")
Study3_MONO_CpdB_Conc <- export_conc_mono_doses(study3,"CpdB")

# Combination 
Study3_COMBI_CpdA_Conc <- export_conc_combi_doses(study3,"CpdA")
Study3_COMBI_CpdB_Conc <- export_conc_combi_doses(study3,"CpdB")

# Fit monotherapy:
Study1GG <- ggplot()+
  geom_point(data=Study1_CpdA_Conc,aes(x=NT,y=DV,color=as.factor(Dose)))


Study1GG <- ggplot()+
  geom_smooth(data=Study1_CpdA_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="glm",se=FALSE)+
  scale_y_log10()+
  theme_bw()


Study2GG <- ggplot()+
  geom_point(data=Study2_CpdB_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)))+
  geom_smooth(data=Study2_CpdB_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="gam",formula = y ~ x + I(x^2),se=FALSE)+
  scale_y_log10()+
  theme_bw()


Study2GG <- ggplot()+
  geom_point(data=Study2_CpdB_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)))+
  geom_smooth(data=Study2_CpdB_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="loess",se=FALSE)+
  scale_y_log10()+
  theme_bw()

Study3GGA <- ggplot()+
  geom_point(data=
               Study3_COMBI_CpdA_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)))+
  geom_smooth(data=
                Study3_COMBI_CpdA_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="gam",formula = y ~ x + I(x^2),se=FALSE)+
  scale_y_log10()+
  theme_bw()+
  coord_cartesian(ylim=c(0.001,10))

Study1GGA <- ggplot()+
  geom_point(data=
               filter(study1_conc,Dose==5|Dose==10|Dose==25),aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)))+
  geom_smooth(data=
                filter(study1_conc,Dose==5|Dose==10|Dose==25),aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="gam",formula = y ~ x + I(x^2),se=FALSE)+
  scale_y_log10()+
  theme_bw()+
  coord_cartesian(ylim=c(0.001,10))


Study3GGA <- ggplot()+
  geom_point(data=
               Study3_COMBI_CpdA_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)))+
  geom_smooth(data=
                Study3_COMBI_CpdA_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="glm",se=FALSE)+
  scale_y_log10()+
  theme_bw()+
  coord_cartesian(ylim=c(0.001,10))

Study1GGA <- ggplot()+
  geom_point(data=
               filter(study1_conc,Dose==5|Dose==10|Dose==25),aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)))+
  geom_smooth(data=
                filter(study1_conc,Dose==5|Dose==10|Dose==25),aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="glm",se=FALSE)+
  scale_y_log10()+
  theme_bw()+
  coord_cartesian(ylim=c(0.001,10))

Study2GG <- ggplot()+
  geom_smooth(data=Study2_CpdB_Conc,aes(x=NT,y=DV,group=as.factor(Dose),color=as.factor(Dose)),method="loess",se=FALSE)+
  scale_y_log10()+
  theme_bw()


Study2GG <- ggplot()+
  geom_line(data=Study2_CpdB_Conc,aes(x=NT,y=DV,color=as.factor(ID)))+
  scale_y_log10()+
  theme_bw()


Study1GG <- ggplot()+
  geom_line(data=Study1_CpdA_Conc,aes(x=NT,y=DV,color=as.factor(ID)))+
  scale_y_log10()+
  theme_bw()

library(ggbump)

Study2GG <- ggplot()+
  geom_sigmoid(data=Study2_CpdB_Conc,aes(x=NT,y=DV,color=as.factor(ID),xend=100,yend=100))+
  scale_y_log10()+
  theme_bw()