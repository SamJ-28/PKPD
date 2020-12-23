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
VC =  1 
KA = 1 

sigma1 = 0.1 

$ODE

dxdt_ABSCMT = -KA*ABSCMT;
dxdt_MAINCMT = KA*ABSCMT - CL / VC  *MAINCMT;

$TABLE
  
$CAPTURE
'

mod<-mcode(model="modeltest",code=code)

# Read in drug concs:
Study1 <- get_individual_study(PKPDdata,"Study1")
# Drug conc 
Study1_CpdA_Conc <- export_conc_mono(Study1,"CpdA")

# Read in drug dose
