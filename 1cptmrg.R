library(mrgsolve)
library (dplyr)
library (nloptr)
library (ggplot2)


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
