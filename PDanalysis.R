# Script to investigate PD parameters 

library(tidyverse)
library(ggplot2)
library(sicegar)

# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

# Use get par to pull in parasitaemia data, which is % of parasitized eryhtrocytes 
# Pull in studies now.. 
study1 <- get_individual_study(PKPDdata,"Study1")
study2 <- get_individual_study(PKPDdata,"Study2")
study3 <- get_individual_study(PKPDdata,"Study3")

study1CpdA_par <- get_par(study1,"CpdA")
study2CpdB_par <- get_par(study2,"CpdB")

study1CpdA_par <- get_par(study1,"CpdA")
study2CpdB_par <- get_par(study2,"CpdB")

# These are the "monotherapy" of CpdA in Study 3
study3CpdA_par <- get_par(study3,"CpdA")
# "Monotherapy" of CpdB in Study 3
study3CpdB_par <- get_par(study3,"CpdB")
# These are the combination runs 
study3Combi_par <- get_par(study3,"Combination")

# pull in plot function 

spaghetti_par <- function(Study){
  
  print("Takes get_par")
  gg_conc <- ggplot()+
    geom_point(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID),group=as.factor(Study$ID)))+
    theme_bw()
  
  return(gg_conc)
}

# Facets are a nice way of splitting the very dense Study3 data.. 
# Present these as the graphical interpretation of parasitaemia data

spaghetti_par(study3Combi_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 3 (Combination therapy)")

#ggsave("Study3_par.tiff")

spaghetti_par(study1CpdA_par)+
  facet_grid(DOSECpdA~., labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 1 (Compound A)")


#ggsave("Study1_par.tiff")

spaghetti_par(study2CpdB_par)+
  facet_grid(DOSECpdB~., labeller = label_both)+
  xlab("Time (hours)")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none",strip.text.y = element_text(angle=0))+
  coord_cartesian(ylim=c(-5,5))+
  ggtitle("Log % change in parasitized red blood cells: Study 2 (Compound B)")


#ggsave("Study2_par.tiff")
###################
#====================
###################

# Conc response needs to be single dose i.e. that the response is taken at 23/24 
# Before the second dose.. 

S3CombiA <- conc_response(study3,"CpdA",1)

S3CombiA1Dose <- filter(S3CombiA,TIME==24)


S3CombiB <- conc_response(study3,"CpdB",1)

S3CombiB1Dose <- filter(S3CombiB,TIME==24)

S1A <- conc_response(study1,"CpdA",0)
S1Dose <- filter(S1A,TIME==24)

S2B <- conc_response(study2,"CpdB",0)
S2Dose <- filter(S2B,TIME==24)
##################
# Plots of conc response
##################

S3Aconc_response<-ggplot()+
  geom_point(data=S3CombiA1Dose,aes(y=response,x=conc))+
  coord_cartesian(ylim=c(0.1,5))+
  geom_smooth(data=S3CombiA1Dose,aes(y=response,x=conc),method="glm")+
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Concentration-Response: Study 3, Compound A")

#ggsave("S3a.tiff")

S3Bconc_response<-ggplot()+
  geom_point(data=S3CombiB1Dose,aes(y=response,x=conc))+
  coord_cartesian(ylim=c(-2,2))+
  geom_smooth(data=S3CombiB1Dose,aes(y=response,x=conc),method="glm")+
  ggtitle("Concentration-Response: Study 3, Compound B")+
  scale_x_log10()

#ggsave("S3b.tiff")

S1conc_response<-ggplot()+
  geom_point(data=S1A,aes(y=response,x=conc))+
  geom_smooth(data=S1A,aes(y=response,x=conc),method="glm")+
  coord_cartesian(ylim=c(-3,3))+
  ggtitle("Concentration-Response: Study 1, Compound A")+
  scale_x_log10()
  

#ggsave("S1a.tiff")

S2conc_response<-ggplot()+
  geom_point(data=S2B,aes(y=response,x=conc))+
  geom_smooth(data=S2B,aes(y=response,x=conc),method="glm")+
  coord_cartesian(ylim=c(-4,4))+
  ggtitle("Concentration-Response: Study 2, Compound B")+
  scale_x_log10()


#ggsave("S2b.tiff")

# Pull in dose to double check 
S1AmonoDose <- get_dose_mono(study1,"CpdA")
S3AmonoDose <- get_dose_combi(study3,"CpdA",1)

##
# Conc response sigmoidal using sicegar
# Not sure if this is appropriate but seems to be a good example for the purpose of the interview. 
# https://cran.r-project.org/web/packages/sicegar/vignettes/introduction.html
# Takes first dose data (see above)

sigmoid_fit <- function(conc_response_data,untransform){
  
  sigmoid_df <- conc_response_data %>%
    select(conc,response)
  
  if(untransform==1){
    sigmoid_df$response <- exp(sigmoid_df$response)
    sigmoid_df$response <- 1/sigmoid_df$response
  }
  
  # trick sicegar into thinking we're time data 
  colnames(sigmoid_df) <- c("time","intensity")
  
  return(sigmoid_df)
  
  
}

S1ASig <- sigmoid_fit(S1Dose,1)

fitObj <- fitAndCategorize(S1ASig,
                           threshold_minimum_for_intensity_maximum = 0.3,
                           threshold_intensity_range = 0.1,
                           threshold_t0_max_int = 0.05)

fig_1A <- figureModelCurves(dataInput = fitObj$normalizedInput,
                           sigmoidalFitVector = fitObj$sigmoidalModel,
                           showParameterRelatedLines = TRUE)


S3ASig <- sigmoid_fit(S3CombiA1Dose,1)


fitObj <- fitAndCategorize(S3ASig,
                           threshold_minimum_for_intensity_maximum = 0.3,
                           threshold_intensity_range = 0.1,
                           threshold_t0_max_int = 0.05)

fig_3A <- figureModelCurves(dataInput = fitObj$normalizedInput,
                           sigmoidalFitVector = fitObj$sigmoidalModel,
                           showParameterRelatedLines = TRUE)


S2BSig <- sigmoid_fit(S2Dose,1)
S3BSig <- sigmoid_fit(S3CombiB1Dose,1)
####
# Plot for PRRs : 

Compound_A_PRR_conc <- ggplot()+
  geom_point(data=S1ASig, aes(x=time,y=intensity),color="blue")+
  geom_point(data=S3ASig, aes(x=time,y=intensity),color="orange")


Compound_B_PRR_conc <- ggplot()+
  geom_point(data=S2BSig, aes(x=time,y=intensity),color="blue")+
  geom_point(data=S3BSig, aes(x=time,y=intensity),color="orange")

## Untransformed i.e. ln % 

Compound_A_conc_ln <- ggplot()+
  geom_point(data=S1A, aes(x=conc,y=response),color="blue")+
  geom_point(data=S3CombiA1Dose, aes(x=conc,y=response),color="orange")


Compound_B_conc_ln <- ggplot()+
  geom_point(data=S2B, aes(x=conc,y=response),color="blue")+
  geom_point(data=S3CombiB1Dose, aes(x=conc,y=response),color="orange")

# Response-dose... 
S1ADoseResponse <- doseResponse24_mono(study1,"CpdA")%>%
  mutate(study="Monotherapy")

S2BDoseResponse <- doseResponse24_mono(study2,"CpdB")%>%
  mutate(study="Monotherapy")

S3ADoseResponse <- doseResponse24_s3(study3,"CpdA")%>%
  mutate(study="Combination")

S3BDoseResponse <- doseResponse24_s3(study3,"CpdB")%>%
  mutate(study="Combination")

CpdADoseResponse <-rbind(S1ADoseResponse,S3ADoseResponse)
CpdBDoseResponse <-rbind(S2BDoseResponse,S3BDoseResponse)

Compound_A_DoseResponse <- ggplot()+
  geom_point(data=CpdADoseResponse,aes(x=dose,y=response,color=study))+
  ylab("Log % of parasitized erythrocytes")+
  xlab("Dose (mg/kg)")+
  ggtitle("Dose-response of compound A at 24h after a single dose")+
  theme_bw()

#ggsave("CpdADR_log.tiff")

Compound_B_DoseResponse <- ggplot()+
  geom_point(data=CpdBDoseResponse,aes(x=dose,y=response,color=study))+
  ylab("Log % of parasitized erythrocytes")+
  xlab("Dose (mg/kg)")+
  ggtitle("Dose-response of compound B at 24h after a single dose")+
  theme_bw()

#ggsave("CpdBDR_log.tiff")

# Change to PRR 24 assuming % irbc = 1 at time 0..
S1A_PRR <- S1ADoseResponse
S2B_PRR <- S2BDoseResponse

S3A_PRR <- S3ADoseResponse
S3B_PRR <- S3BDoseResponse

S1A_PRR$response <- 1/(exp(S1A_PRR$response))
S2B_PRR$response <- 1/(exp(S2B_PRR$response))

S3A_PRR$response <- 1/(exp(S3A_PRR$response))
S3B_PRR$response <- 1/(exp(S3B_PRR$response))

CpdA_PRR <- rbind(S1A_PRR,S3A_PRR)

CpdB_PRR <- rbind(S2B_PRR,S3B_PRR)


Compound_A_PRR <- ggplot()+
  geom_point(data=CpdA_PRR,aes(x=dose,y=response,color=study))+
  ylab("Parasite reduction ratio at 24h")+
  xlab("Dose (mg/kg)")+
  ggtitle("Dose-response of compound A at 24h after a single dose")+
  coord_cartesian(ylim=c(0.01,4))+
  theme_bw()
#ggsave("CpdAPRR.tiff")

Compound_B_PRR <- ggplot()+
  geom_point(data=CpdB_PRR,aes(x=dose,y=response,color=study))+
  ylab("Parasite reduction ratio at 24h")+
  xlab("Dose (mg/kg)")+
  ggtitle("Dose-response of compound B at 24h after a single dose")+
  coord_cartesian(ylim=c(0.01,4))+
  theme_bw()

#ggsave("CpdBPRR.tiff")



#####
# Extra anlaysis to compare response on the same plots 

# Get unique doses of A and B for combi 
combi_a <- unique(study3Combi_par$DOSECpdA)
combi_b <- unique(study3Combi_par$DOSECpdB)

subset_mono_1 <- filter(study1CpdA_par, DOSECpdA%in%combi_a)%>%
  select(ID,NT,DV,DOSECpdA,DOSECpdB)%>%
  mutate(study="monotherapy")

subset_mono_2 <- filter(study2CpdB_par, DOSECpdB%in%combi_b)%>%
  select(ID,NT,DV,DOSECpdA,DOSECpdB)%>%
  mutate(study="monotherapy")

subset_combi_A <- select(study3Combi_par, ID,NT,DV,DOSECpdA,DOSECpdB)%>%
  mutate(study="combination")

subset_combi_B <- select(study3Combi_par, ID,NT,DV,DOSECpdA,DOSECpdB)%>%
  mutate(study="combination")

all_A <- rbind(subset_mono_1,subset_combi_A)
all_B <- rbind(subset_mono_2,subset_combi_B)

CpdA_response <- ggplot(data=all_A,aes(x=NT,y=DV,group=ID,color=study))+
  geom_line()+
  coord_cartesian(ylim=c(-6,6))+
  facet_grid(DOSECpdA~DOSECpdB, labeller=label_both)+
  xlab("Time (hours)")+
  ylab("Log % of Parasitized Erythrocytes")+
  ggtitle("Response-time curves: Compound A")+
  theme_bw()

ggsave("Response_CpdA.tiff")

CpdB_response <- ggplot(data=all_B,aes(x=NT,y=DV,group=ID,color=study))+
  geom_line()+
  coord_cartesian(ylim=c(-6,6))+
  facet_grid(DOSECpdA~DOSECpdB, labeller=label_both)+
  xlab("Time (hours)")+
  ylab("Log % of Parasitized Erythrocytes")+
  ggtitle("Response-time curves: Compound B")+
  theme_bw()

ggsave("Response_CpdB.tiff")
# Convert to parasite % for geo means assuming starting P of 1 
PR_A_mono <- subset_mono_1
PR_B_mono <- subset_mono_2

PR_A_combi <- subset_combi_A
PR_B_combi <- subset_combi_B

PR_A_mono$DV <- (exp(PR_A_mono$DV))
PR_B_mono$DV <- (exp(PR_B_mono$DV))

PR_A_combi$DV <- (exp(PR_A_combi$DV))
PR_B_combi$DV <- (exp(PR_B_combi$DV))

geo_A_mono <- get_geo_mean(PR_A_mono)%>%
  mutate(study="monotherapy")
geo_B_mono <- get_geo_mean(PR_B_mono)%>%
  mutate(study="monotherapy")

geo_A_combi <- get_geo_mean(PR_A_combi)%>%
  mutate(study="combination")
geo_B_combi <- get_geo_mean(PR_B_combi)%>%
  mutate(study="combination")
