# Script to investigate PD parameters 

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

spaghetti_par(study3Combi_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")

spaghetti_par(study1CpdA_par)+
  facet_grid(DOSECpdA~., labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  theme(legend.position = "none")+
  coord_cartesian(ylim=c(-5,5))

# Fit glm to log parasitaemia? 
study1_glm <- ggplot()+
  geom_point(data=study1CpdA_par,aes(y=DV,x=NT,color=as.factor(study1CpdA_par$DOSECpdA)))+
  geom_smooth(data=study1CpdA_par,aes(y=DV,x=NT,color=as.factor(study1CpdA_par$DOSECpdA),group=as.factor(study1CpdA_par$DOSECpdA)),method="glm",se=FALSE)
  

<<<<<<< HEAD
=======


>>>>>>> 1c64ab91ff0d4620c2dbe1c5a784ec63acca3138
###################
#====================
spaghetti_par(study3CpdA_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")


spaghetti_par(study3CpdB_par)+
  facet_grid(DOSECpdA~DOSECpdB, labeller = label_both)+
  xlab("Time")+
  ylab("Log % change in parasitized red blood cells")+
  geom_vline(xintercept=48)+
  theme(legend.position = "none")
  
# Function takes output of get_par. 
# Hashed out parts can return mean / median etc.. 
# Just calculates response at 48h and relates to dose. May not be useful.. 
doseResponse48 <- function(data,which_compound){
  
  data48 <- data %>%
    filter(TIME==48)
  
  if(which_compound=="CpdA"){
   doses <- unique(data48$DOSECpdA)
  }
  
  if(which_compound=="CpdB"){
    doses <- unique(data48$DOSECpdB)
  }
  if(which_compound=="Combination"){
    data48 <- data48%>%
      mutate(combined_dose=DOSECpdA+DOSECpdB)
    
    doses <- unique(data48$combined_dose)
  }
  
  individual_data <- data.frame(dose=NA,response=NA)
  # Loop through each dose for the values of parasitaemia
  
  response<-c()
  for(i in doses){
    
    if(which_compound=="CpdA"){
      dosesubset <- data48%>%
        filter(DOSECpdA==i)
    }
    
    if(which_compound=="CpdB"){
      dosesubset <- data48%>%
        filter(DOSECpdB==i)
    }
    if(which_compound=="Combination"){
      dosesubset <- data48%>%
        filter(combined_dose==i)
    }

    individual_data_bind <- data.frame(dose=rep(i,times=length(dosesubset$DV)),response=dosesubset$DV)
    dose_mean<- mean(dosesubset$DV)
    
    response<-c(response,dose_mean)
    
    
    individual_data <- rbind(individual_data,individual_data_bind)
  }
  
  
  dose_response <- data.frame(Dose=doses,Response=response) %>%
    arrange(Dose)
  
  # Return dose_response if you want the mean.. 
  #return(dose_response)
  individual_data<-individual_data[-1,]
  return(individual_data)
}  


monoA <- doseResponse48(study3CpdA_par,"CpdA")
actualA <- doseResponse48(study1CpdA_par,"CpdA")

combiA<-doseResponse48(study3Combi_par,"CpdA")

monoB <- doseResponse48(study3CpdB_par,"CpdB")
actualB <- doseResponse48(study2CpdB_par,"CpdB")

print("marker")
combiB<-doseResponse48(study3Combi_par,"CpdB")

#combi_add <- doseResponse48(study3Combi_par,"Combination")

dose_response <- ggplot()+
  geom_smooth(data=actualA,aes(y=Response,x=Dose,color="Red"),method=lm )+ 
  geom_smooth(data=combiA,aes(y=Response,x=Dose,color="Blue"),method=lm) 

# Actually, if i'm fitting lm() there's no need to take an average. 
# Note that this is a log-linear regression mode... 

dose_response<-ggplot()+
  geom_point(data=rbind(actualA,monoA),aes(y=response,x=dose))+
  geom_point(data=combiA,aes(y=response,x=dose))+
  geom_smooth(data=rbind(actualA,monoA),aes(y=response,x=dose),method="loess")+
  geom_smooth(data=combiA,aes(y=response,x=dose),method="loess")+
  scale_x_log10()


dose_response<-ggplot()+
  geom_point(data=rbind(actualB,monoB),aes(y=response,x=dose))+
  geom_point(data=combiB,aes(y=response,x=dose))+
  geom_smooth(data=rbind(actualB,monoB),aes(y=response,x=dose),method="glm")+
  geom_smooth(data=combiB,aes(y=response,x=dose),method="glm")+
  scale_x_log10()

# Also need conc response. 
# So need conc for each patient.. 
# Use the export conc functions. 
# Need to link conc at a certain time to response at a certain time. 

# study is the result of get_individual_study.. 

# This might be tricky. I'm reluctant to fudge time-points. 
# Can we NA missings? 
# This function might not be useful. 
conc_response <- function(study,which_compound,combination){
  
  # Response is pulled by getpar. 
   
  
  # For the same study and compound, pull conc.. 
  if(combination==0){
    conc <- export_conc_mono(study,which_compound)
    response <- get_par(study,which_compound)
  }
  
  if(combination==1){
    conc <- export_conc_combi(study,which_compound)
    response <- get_par(study,"Combination")
  }
  
  trim_response <- response %>%
    select(ID,NT,DV)
  
  # conc is "pre-trimmed". 
  IDs <- unique(conc$ID)
  
  
  study_response_conc <- data.frame(TIME=NA,ID=NA,conc=NA,response=NA)
  for(i in IDs){
    
    conc_subset <- filter(conc,ID==i)
    response_subset <- filter(trim_response, ID==i)
    
    # Union or intersect? Either way... 
    # Also alters... hrm. 
    
    # MODIFIES 23 TO 24... BAD WORKAROUND
    conc_subset$NT[which(conc_subset$NT==23.0)]<-24
    
    time_intersect <- intersect(conc_subset$NT,response_subset$NT)
    
    # For each portion of time intersect, we store data at that time for conc and response for the ID.. 
    for(t in time_intersect){
      
      intersect_data <- data.frame(TIME=t, ID=i, conc=filter(conc_subset,NT==t)$DV,response=filter(response_subset,NT==t)$DV)
      
      study_response_conc <- rbind(study_response_conc,intersect_data)
    }
  }
  study_response_conc <- study_response_conc[-1,]
  return(study_response_conc)
}

S3CombiA <- conc_response(study3,"CpdA",1)

S3CombiA1Dose <- filter(conc_response(study3,"CpdA",1),TIME==24)

S1A <- conc_response(study1,"CpdA",0)
S1Dose <- filter(S1A,TIME==24)


conc_response<-ggplot()+
  geom_smooth(data=S3CombiA1Dose,aes(y=response,x=conc),method="glm",se=FALSE)+
  coord_cartesian(ylim=c(-1,1))

conc_response<-ggplot()+
  geom_point(data=S3CombiA1Dose,aes(y=response,x=conc))

conc_response<-ggplot()+
  geom_smooth(data=S1Dose,aes(y=response,x=conc),method="glm",se=FALSE)+
  coord_cartesian(ylim=c(-1,1))

# Pull in dose to double check 
S1AmonoDose <- get_dose_mono(study1,"CpdA")
S3AmonoDose <- get_dose_combi(study3,"CpdA",1)

# TODO: Response is time 24... but conc isn't. 