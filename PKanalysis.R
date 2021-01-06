# Script to compare and investigate PK parameters / predictions from mrgmodels 
# Running the mrg_model functions can be weird if the models (i.e., mod<-modlib(x)) aren't loaded!

#################
# If errors are returned, go into functions.R and manualy run the mod<-modlib(x) lines within mrg_model().
# This should then allow this script to be sourced and mrg_model to run properly. 
# I couldn't work out a fix without losing the functionalized nature of this code. 

#################
# Such errors will look like: "There was a problem accessing the model shared object.
# Either the model object is corrupted or the model was 
# not properly compiled and/or loaded
#################

# Code for the mrg models draws heavily from Metrum github resources:
# https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md

# Note: This script will take a long time to run everything. 
# I recommend running one-by-one the sections you're interested in. 
# Unfortunately, I have made this inefficient as mrg_model has to be run once for pred and once for param
# An obvious improvement would be to store them in an array. 

# Another improvement would be adjusting the model with minimum values of parameters to stop the minus numbers
# Again, timing issues.

library(mrgsolve)
library (dplyr)
library (nloptr)
library (ggplot2)

# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")


########
# Examples for presentation
########
# 2 comp:
# This was the first model I wrote so the labelling for 2cmt is essentially null, whereas 1cmt has _1 tag on objects

Study1ParamLWS <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"LWS","params")
Study1PredLWS <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"LWS","pred")

Study1ParamOLS <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS","params")
Study1PredOLS <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS","pred")

Study1ParamOLSmin <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS_min","params")
Study1PredOLSmin <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS_min","pred")


Study1_conc <- export_conc_mono_doses(get_individual_study(PKPDdata,"Study1"),"CpdA")%>%
  mutate(method="Concentration")

Study1_geomean<-get_geo_mean(Study1_conc)

Study1PredOLS <- drop_na(Study1PredOLS)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="OLS")%>%
  relocate(ID,NT,DV,Dose,method)


Study1PredLWS <- drop_na(Study1PredLWS)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)


Study1PredOLSmin <- drop_na(Study1PredOLSmin)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="OLS_min")%>%
  relocate(ID,NT,DV,Dose,method)

# Can get geomean on these now

Study1OLS_geo <- get_geo_mean(Study1PredOLS)
Study1LWS_geo <- get_geo_mean(Study1PredLWS)
Study1OLSmin_geo <- get_geo_mean(Study1PredOLSmin)

Study1_2cmt <- ggplot()+
  geom_line(data=Study1_geomean,aes(x=NT,y=GeoMean))+
  geom_line(data=Study1OLS_geo,aes(x=NT,y=GeoMean),color="red",linetype="dashed")+
  geom_line(data=Study1LWS_geo,aes(x=NT,y=GeoMean),color="green",linetype="dashed")+
  geom_line(data=Study1OLSmin_geo,aes(x=NT,y=GeoMean),color="blue",linetype="dashed")+
  facet_grid(.~Dose)+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()+
  labs(x="Time")+
  ggtitle("Geometric means of in vitro concentration and mrgsolve predicted concentrations",subtitle="Study 1, Compound A, 2 compartment model")


#######
# 1 comp
#######

Study1ParamLWS1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"LWS","params")
Study1PredLWS1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"LWS","pred")

Study1ParamOLS1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"OLS","params")
Study1PredOLS1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"OLS","pred")

Study1ParamOLSmin1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"OLS_min","params")
Study1PredOLSmin1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"OLS_min","pred")


Study1_conc <- export_conc_mono_doses(get_individual_study(PKPDdata,"Study1"),"CpdA")%>%
  mutate(method="Concentration")

Study1_geomean<-get_geo_mean(Study1_conc)

Study1PredOLS1 <- drop_na(Study1PredOLS1)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="OLS")%>%
  relocate(ID,NT,DV,Dose,method)
  

Study1PredLWS1 <- drop_na(Study1PredLWS1)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)


Study1PredOLSmin1 <- drop_na(Study1PredOLSmin1)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="OLS_min")%>%
  relocate(ID,NT,DV,Dose,method)

# Can get geomean on these now

Study1OLS1_geo <- get_geo_mean(Study1PredOLS1)
Study1LWS1_geo <- get_geo_mean(Study1PredLWS1)
Study1OLSmin1_geo <- get_geo_mean(Study1PredOLSmin1)

cols <- c()

Study1_1cmt <- ggplot()+
  geom_line(data=Study1_geomean,aes(x=NT,y=GeoMean),color="black")+
  geom_line(data=Study1OLS1_geo,aes(x=NT,y=GeoMean),color="red",linetype="dashed")+
  geom_line(data=Study1LWS1_geo,aes(x=NT,y=GeoMean),color="green",linetype="dashed")+
  geom_line(data=Study1OLSmin1_geo,aes(x=NT,y=GeoMean),color="blue",linetype="dashed")+
  facet_grid(.~Dose,labeller=label_both)+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()+
  labs(x="Time")+
  ggtitle("Geometric means of in vitro concentration and mrgsolve predicted concentrations",subtitle="Study 1, Compound A, 1 compartment model")

#ggsave("Study1_preds_1cmt.tiff")
#######
# Testing compartments: 
#######
# Want to compare Study 3 with 1 and 2, basically. I guess LWS is the best optimizer?? 

Study3concACombi <- export_conc_combi_doses(get_individual_study(PKPDdata,"Study3"),"CpdA")
Study3PredACombi_2 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"LWS","pred")

Study3concBCombi <- export_conc_combi_doses(get_individual_study(PKPDdata,"Study3"),"CpdB")
Study3PredBCombi_2 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),2,"LWS","pred")


Study3PredACombi_1 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),1,"LWS","pred")
Study3PredACombi_3 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),3,"LWS","pred")

Study3PredBCombi_1 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),1,"LWS","pred")
Study3PredBCombi_3 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),3,"LWS","pred")

Study3_A_geo_conc <- get_geo_mean(Study3concACombi)
Study3_B_geo_conc <- get_geo_mean(Study3concBCombi)


# 1 cmt
Study3_A_pred_1 <- drop_na(Study3PredACombi_1)%>%
  add_dose_data(conc=Study3concACombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)

Study3_B_pred_1 <- drop_na(Study3PredBCombi_1)%>%
  add_dose_data(conc=Study3concBCombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)

# 2 cmt

Study3_A_pred_2 <- drop_na(Study3PredACombi_2)%>%
  add_dose_data(conc=Study3concACombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)

Study3_B_pred_2 <- drop_na(Study3PredBCombi_2)%>%
  add_dose_data(conc=Study3concBCombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)

Study3_A_geo_2 <- get_geo_mean(Study3_A_pred_2)
Study3_B_geo_2 <- get_geo_mean(Study3_B_pred_2)

Study3_A_geo_1 <- get_geo_mean(Study3_A_pred_1)
Study3_B_geo_1 <- get_geo_mean(Study3_B_pred_1)

Study3_A <- ggplot()+
  geom_line(data=Study3_A_geo_conc,aes(x=NT,y=GeoMean))+
  geom_line(data=Study3_A_geo_2,aes(x=NT,y=GeoMean),color="red",linetype="dotted",size=1)+
  geom_line(data=Study3_A_geo_1,aes(x=NT,y=GeoMean),color="blue",linetype="dotted",size=1)+
  facet_grid(.~Dose)+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()+
  labs(x="Time")+
  ggtitle("Geometric means of in vitro concentration and mrgsolve predicted concentrations",subtitle="Study 3, Compound A")


#ggsave("Study3_A.tiff")

Study3_B <- ggplot()+
  geom_line(data=Study3_B_geo_conc,aes(x=NT,y=GeoMean))+
  geom_line(data=Study3_B_geo_2,aes(x=NT,y=GeoMean),color="red",linetype="dotted",size=1)+
  geom_line(data=Study3_B_geo_1,aes(x=NT,y=GeoMean),color="blue",linetype="dotted",size=1)+
  facet_grid(.~Dose)+
  scale_y_continuous(trans="log10",name="Concentration of drug (ug/mL)")+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()+
  labs(x="Time")+
  ggtitle("Geometric means of in vitro concentration and mrgsolve predicted concentrations",subtitle="Study 3, Compound B")

#ggsave("Study3_B.tiff")

# Try 3 cmt?
Study3PredA_3 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),3,"LWS","pred")


Study3PredA_3_v <- drop_na(Study3PredA_3)%>%
  add_dose_data(conc=Study3concACombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)

Study3_3cmt_geo <- get_geo_mean(Study3PredA_3_v)

Study3_A_3cmt <- ggplot()+
  geom_line(data=Study3_A_geo_conc,aes(x=NT,y=GeoMean))+
  geom_line(data=Study3_3cmt_geo,aes(x=NT,y=GeoMean),color="red",linetype="dotted",size=1)+
  facet_grid(.~Dose)+
  scale_y_log10()+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()

########
# Parmameter comparisons between mono / combi:
########

Study1_conc <- export_conc_mono_doses(get_individual_study(PKPDdata,"Study1"),"CpdA")%>%
  mutate(method="Concentration")

Study2_conc <- export_conc_mono_doses(get_individual_study(PKPDdata,"Study2"),"CpdB")%>%
  mutate(method="Concentration")

Study1Pred_CPDA_2_min <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS_min","pred")

Study2Pred_CPDB_2_min <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"OLS_min","pred")

Study1PredA_min <- drop_na(Study1Pred_CPDA_2_min)%>%
  add_dose_data(conc=Study1_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="OLS_min")%>%
  relocate(ID,NT,DV,Dose,method)

Study2PredB_min <- drop_na(Study2Pred_CPDB_2_min)%>%
  add_dose_data(conc=Study2_conc)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="OLS_min")%>%
  relocate(ID,NT,DV,Dose,method)


Study1_A_min_geo <- get_geo_mean(Study1PredA_min)

Study2_B_min_geo <- get_geo_mean(Study2PredB_min)


Study1_geomean<-get_geo_mean(Study1_conc)

Study2_geomean<-get_geo_mean(Study2_conc)

Study1_min <- ggplot()+
  geom_line(data=Study1_geomean,aes(x=NT,y=GeoMean))+
  geom_line(data=Study1_A_min_geo,aes(x=NT,y=GeoMean),color="red",linetype="dotted",size=1)+
  facet_grid(.~Dose)+
  scale_y_log10()+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()

Study2_min <- ggplot()+
  geom_line(data=Study2_geomean,aes(x=NT,y=GeoMean))+
  geom_line(data=Study2_B_min_geo,aes(x=NT,y=GeoMean),color="red",linetype="dotted",size=1)+
  facet_grid(.~Dose)+
  scale_y_log10()+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()

# Now compare params

Study1Param_CPDA_2 <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"LWS","params")
Study2Param_CPDB_2 <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"LWS","params")


Study3Param_A_2 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"LWS","params")
Study3Param_B_2 <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),2,"LWS","params")

Study1Param_CPDA_2_min <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS_min","params")
Study2Param_CPDB_2_min <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"OLS_min","params")

# Test modified Dosing:


Study1_mod <- get_mrgdata_12("Study1","CpdA")
Study1_mod$amt <- Study1_mod$amt*1000

ModA <- mrg_model(Study1_mod,2,"LWS","pred")
