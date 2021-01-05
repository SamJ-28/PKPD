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
  scale_y_log10()+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()

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

Study1_1cmt <- ggplot()+
  geom_line(data=Study1_geomean,aes(x=NT,y=GeoMean))+
  geom_line(data=Study1OLS1_geo,aes(x=NT,y=GeoMean),color="red",linetype="dotted",size=1)+
  geom_line(data=Study1LWS1_geo,aes(x=NT,y=GeoMean),color="green",linetype="dashed")+
  geom_line(data=Study1OLSmin1_geo,aes(x=NT,y=GeoMean),color="blue",linetype="dashed")+
  facet_grid(.~Dose)+
  scale_y_log10()+
  coord_cartesian(ylim=c(0.000001,10))+
  theme_bw()
  
#######
# 2 comp works better...
#######
# Want to compare Study 3 with 1 and 2, basically. I guess LWS is the best optimizer?? 

Study3concACombi <- export_conc_combi_doses(get_individual_study(PKPDdata,"Study3"),"CpdA")
Study3PredACombi <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"LWS","pred")

Study3concBCombi <- export_conc_combi_doses(get_individual_study(PKPDdata,"Study3"),"CpdB")
Study3PredBCombi <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),2,"LWS","pred")

Study3_A_pred <- drop_na(Study3PredACombi)%>%
  add_dose_data(conc=Study3concACombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)


Study3_B_pred <- drop_na(Study3PredBCombi)%>%
  add_dose_data(conc=Study3concBCombi)%>%
  select(time,ID,mrgpred,Dose)%>%
  rename(NT=time,DV=mrgpred)%>%
  mutate(method="LWS")%>%
  relocate(ID,NT,DV,Dose,method)

########
# Additional runs
########

# Study 1

Study1Param_1cmt <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"LWS","params")
Study1Pred_1cmt <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"LWS","pred")

Study1Param_2cmt <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"LWS","params")
Study1Pred_2cmt <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"LWS","pred")


Study1Param_OLS <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS","params")
Study1Pred_OLS <-mrg_model(get_mrgdata_12("Study1","CpdA"),2,"OLS","pred")


Study1Param_OLS1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"OLS","params")
Study1Pred_OLS1 <-mrg_model(get_mrgdata_12("Study1","CpdA"),1,"OLS","pred")

# Study 2
Study2Param_1cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),1,"LWS","params")
Study2Pred_1cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),1,"LWS","pred")

Study2Param_2cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"LWS","params")
Study2Pred_2cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"LWS","pred")

# Read in concentration data

Study1conc <- get_conc3(get_individual_study(PKPDdata,"Study1"))
# Although in all honesty the mrg functions might be better i.e. 
Study1conc2 <- export_conc_mono(get_individual_study(PKPDdata,"Study1"),"CpdA")

 
Study2conc2 <- export_conc_mono(get_individual_study(PKPDdata,"Study2"),"CpdB")

# We want to visually compare the actual conc and the predictions, so i.e.:
# Notes: using conc from study_pred output is awkward as need to remove EVID. 


gg_conc <- ggplot()+
  geom_line(data=Study1conc2,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study1Pred_2cmt,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


gg_conc <- ggplot()+
  geom_line(data=Study1conc2,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study1Pred_1cmt,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


gg_conc <- ggplot()+
  geom_line(data=Study2conc2,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study2Pred_2cmt,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


gg_conc <- ggplot()+
  geom_line(data=Study2conc2,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study2Pred_1cmt,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()

# Study 3.. 

Study3concAMono <- export_conc_mono(get_individual_study(PKPDdata,"Study3"),"CpdA")

Study3PredAMono <-mrg_model(get_mrgdata_3("Study3","CpdA","CpdA"),2,"LWS","pred")


Study3concBMono <- export_conc_mono(get_individual_study(PKPDdata,"Study3"),"CpdB")

Study3PredBMono <-mrg_model(get_mrgdata_3("Study3","CpdB","CpdB"),2,"LWS","pred")



Study3concACombi <- export_conc_combi(get_individual_study(PKPDdata,"Study3"),"CpdA")

Study3PredACombi <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"LWS","pred")


Study3concBCombi <- export_conc_combi(get_individual_study(PKPDdata,"Study3"),"CpdB")

Study3PredBCombi <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),2,"LWS","pred")

# Params


Study3ParamAMono<-mrg_model(get_mrgdata_3("Study3","CpdA","CpdA"),2,"LWS","params")
Study3ParamBMono<-mrg_model(get_mrgdata_3("Study3","CpdB","CpdB"),2,"LWS","params")

Study3ParamACombi <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"LWS","params")
Study3ParamBCombi <-mrg_model(get_mrgdata_3("Study3","Combination","CpdB"),2,"LWS","params")


gg_conc <- ggplot()+
  geom_line(data=Study3concAMono,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study3PredAMono,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


gg_conc <- ggplot()+
  geom_line(data=Study3concBMono,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study3PredBMono,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


gg_conc <- ggplot()+
  geom_line(data=Study3concACombi,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study3PredACombi,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


gg_conc <- ggplot()+
  geom_line(data=Study3concBCombi,aes(x=NT,y=DV,group=ID,color=as.factor(ID)))+
  geom_line(data=Study3PredBCombi,aes(x=time,y=mrgpred,group=ID,color=as.factor(ID)),linetype="dashed")+
  facet_grid(ID~.)+
  scale_y_log10()


##########
# Repeat with OLS_min 

Study3ParamA_min <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"OLS_min","params")
Study3PredA_min <-mrg_model(get_mrgdata_3("Study3","Combination","CpdA"),2,"OLS_min","pred")
