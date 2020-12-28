# Script to compare and investigate PK parameters / pred from mrgmodels 


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

# Study 2
Study2Param_1cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),1,"LWS","params")
Study2Pred_1cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),1,"LWS","pred")

Study2Param_2cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"LWS","params")
Study2Pred_2cmt <-mrg_model(get_mrgdata_12("Study2","CpdB"),2,"LWS","pred")

# Read in concentration data
i.e.
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


# So just adapt the mrg model script to give normal conc.. 