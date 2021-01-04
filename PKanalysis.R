# Script to compare and investigate PK parameters / predictions from mrgmodels 
# Running the mrg_model functions can be weird if the models (i.e., mod<-modlib(x)) aren't loaded!
# If errors are returned, go into functions.R and manualy run the mod<-modlib(x) lines within mrg_model().
# This should then allow this script to be sourced and mrg_model to run properly. 

# Code for the mrg models draws heavily from Metrum github resources:
# https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md

# Note: This script will take a long time to run everything. 
# I recommend running one-by-one the sections you're interested in. 
# Unfortunately, I have made this inefficient as mrg_model has to be run once for pred and once for param
# An obvious improvement would be to store them in an array. 

library(mrgsolve)
library (dplyr)
library (nloptr)
library (ggplot2)

# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

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
