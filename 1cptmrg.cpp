$PROB
# 1 compartment model for given compound. 
- Sam Jones

$CMT

ABSCMT : Absorption compartment
MAINCMT : Main compartment

$PARAM @annotated

CL: 1 : Clearnace
VC: 1 : Volume of distribution
KA: 1 : Absorption

sigma1: 0.1 : proportional residual 

$ODE

dxdt_ABSCMT = -KA*ABSCMT;
dxdt_MAINCMT = KA*ABSCMT - CL/VC*MAINCMT;

$TABLE
  
$CAPTURE
