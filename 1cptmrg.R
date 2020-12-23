library(mrgsolve)
library (dplyr)
library (nloptr)
library (ggplot2)

# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

code <- '
$PROB
# 1 compartment model for given compound. 
- Sam Jones

$CMT

ABSCMT
MAINCMT

$PARAM 

CL = 1 
VD =  1 
KA = 1 

sigma1 = 0.1 

$ODE

dxdt_ABSCMT = -KA*ABSCMT;
dxdt_MAINCMT = KA*ABSCMT - CL / VD  *MAINCMT;

$TABLE

double Kel = CL/VD;
double Conc = MAINCMT / VD;

double varConc = (Conc*sigma1)*(Conc*sigma1);
  
$CAPTURE CL VD KA Conc varConc

'

mod<-mcode(model="modeltest",code=code)

# Read in drug concs:
Study1 <- get_individual_study(PKPDdata,"Study1")
# Drug conc 
Study1_CpdA_Conc <- export_conc_mono(Study1,"CpdA")

# Read in drug dose
Study1_dose <- get_dose_mono(Study1,"CpdA")

# Extract data for 1 patient: 
conc_9 <- filter(Study1_CpdA_Conc,ID==9)
dose_9 <- filter(Study1_dose,ID==9)
colnames(dose_9)<-c("ID","time","amt")