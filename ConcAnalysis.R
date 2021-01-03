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

##################
# Comparison of conc between Study 3 and 1/2 : means
##################
library(EnvStats)

dose_times <- c(0,24,48,72)


## This format can be used for any of the above data 

Study1GeoMean <- ggplot()+
  geom_point(data=Study1_CpdA_Conc,aes(x=NT,y=DV,color=as.factor(Dose)))+
  geom_line(data=get_geo_mean(Study1_CpdA_Conc),aes(x=NT,y=GeoMean,color=as.factor(Dose),group=as.factor(Dose)))+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  theme_bw()+
  ggtitle("Study 1, concentration of compound A")+
  labs(color="Dose amount (mg/kg)")+
  geom_vline(xintercept=dose_times,linetype="dashed")

# Only Study 1 is shown in the presentation, but repeat the format below for any of the above d.fs:

Study3GeoMean <- ggplot()+
  geom_point(data=Study3_COMBI_CpdA_Conc,aes(x=NT,y=DV,color=as.factor(Dose)))+
  geom_line(data=get_geo_mean(Study3_COMBI_CpdA_Conc),aes(x=NT,y=GeoMean,color=as.factor(Dose),group=as.factor(Dose)))+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  theme_bw()+
  ggtitle("Study 3, concentration of compound A")+
  labs(color="Dose amount (mg/kg)")+
  geom_vline(xintercept=dose_times,linetype="dashed")

#ggsave("geoMean1.tiff")

S1_geomean <- get_geo_mean(Study1_CpdA_Conc) %>%
  filter(Dose==5|Dose==10|Dose==25)%>%
  mutate(Study="Monotherapy",Compound="Compound A")

S2_geomean <- get_geo_mean(Study2_CpdB_Conc) %>%
  filter(Dose==0.50|Dose==1.00|Dose==0.25|Dose==2.50)%>%
  arrange(Dose) %>%
  mutate(Study="Monotherapy",Compound="Compound B")

S3_CpdA_geomean <-get_geo_mean(Study3_COMBI_CpdA_Conc)%>%
  mutate(Study="Combination",Compound="Compound A")

S3_CpdB_geomean <-get_geo_mean(Study3_COMBI_CpdB_Conc)%>%
  mutate(Study="Combination",Compound="Compound B")

all_studies <- rbind(S1_geomean,S2_geomean,S3_CpdA_geomean,S3_CpdB_geomean)

CpdA_geomean <- ggplot()+
  geom_line(data=filter(all_studies,Compound=="Compound A"),aes(x=NT,y=GeoMean,group=interaction(Study,Dose),linetype=as.factor(Study)))+
  facet_grid(.~Dose,labeller = label_both)+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  geom_vline(xintercept=dose_times,linetype="dashed",color="grey")+
  theme_classic()+
  ggtitle("Geometric mean concentrations of Compound A")+
  labs(linetype="Study")

#ggsave("geoMean_A.tiff")

CpdB_geomean <- ggplot()+
  geom_line(data=filter(all_studies,Compound=="Compound B"),aes(x=NT,y=GeoMean,group=interaction(Study,Dose),linetype=as.factor(Study)))+
  facet_grid(.~Dose,labeller = label_both)+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")+
  geom_vline(xintercept=dose_times,linetype="dashed",color="grey")+
  theme_classic()+
  ggtitle("Geometric mean concentrations of Compound B")+
  labs(linetype="Study")


#ggsave("geoMean_B.tiff")

###############
# For completeness, really should test Study 3 Combi v the Mono data-sets. 

########################
########################

Study3GeoMean <- ggplot()+
  geom_point(data=Study3_COMBI_CpdA_Conc,aes(x=NT,y=DV,color=as.factor(Dose)))+
  geom_line(data=get_geo_mean(Study3_COMBI_CpdA_Conc),aes(x=NT,y=GeoMean,color=as.factor(Dose),group=as.factor(Dose)))+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  scale_x_continuous(name="Time (hours)")


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