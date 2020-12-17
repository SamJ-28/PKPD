# R script to clean and visually analyze data
# Sam Jones

# Todo: Read packages at the start (check this before final version)
library(tidyverse)
library(ggplot2)
library(stringr)

# Hard read data 
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

# Count subjects:
unique(PKPDdata$USUBJID)

# Note to self: Study 1 = Comp A, Study 2 = Comp B, Study 3 = Combination. 
# Note to self: TRTNAME Vehicle? I'm assuming this is placebo, but I'm not familiar with this term. 
# Check with MMV? The linked paper doesn't appear to explicitly define. 
# Edit: Given the lack of dosing it is probably placebo. 
# NB: Is "Patient" appropriate for Mice? Not sure, but force of habit! Sorry if you're reading this and disagree. 

# Ultimately I will be looking at each study individually. Separate data into Study 1, Study 2, Study 3. 
# This probably doesn't need to be in a function but I got the feeling MMV are keen on function-based R. 
# Plus in reality we'd be setting up here to re-use code on other data. 
# grep relies on the data being inputted correctly to begin with which is a flaw in this approach... 

# I'm going to want to trim a lot of this data I imagine, but not yet. 

get_individual_study <- function(data,study){
  print("Input should be Study1,Study2 etc. Relies on data being correctly labelled in input, careful.")
  
  individual_study <- data[grep(study,data$USUBJID),]
  return(individual_study)
}

Study1 <- get_individual_study(PKPDdata,"Study1")
Study2 <- get_individual_study(PKPDdata,"Study2")
Study3 <- get_individual_study(PKPDdata,"Study3")

# Manually check that rows add i.e. has a study been mislabeled. 
nrow(Study1)+nrow(Study2)+nrow(Study3)
nrow(PKPDdata)

# Looks good. 

# Let's start visual analysis. Work-flow: continue through even if things look weird. 
# Interests are (initially) log Par and drug conc. Also need to be clear when doses occurred. 
# Doses are marked by DV == 0 so should be easy to extract, but i'll probably just filter par / conc out anyway so can do it then. 

# For now, we'll just do Study 1. 
# Going to just write a script then functionalize later. 

# n.b for the functionalized version don't call dose with name. 
# Actually the same is true of conc as the labels will be different. 

Study1Dose <- filter(Study1, NAME=="CpdA:::Dose")
Study1Par <- filter(Study1, NAME=="Log(Parasitemia)")
Study1Conc <- filter(Study1, NAME=="CpdA:::Blood:::Concentration")

# Fairly simple task to plot these graphically by patient using ggplot. 
# Want to be sure of the timing on the x axis. Quick look at the data shows maxtime is not eqaul for all patients.
# Additionally, be sure of the records. Per the table in the document:
# TIME = time relative to first dose
# TIMEPOS = time since first record. 

# Seems safe to plot x axis as 0 - maxtime and use TIMEPOS to plot. TIME is relative to the first dose, not the first record.
# It's possible that first dose is given after observations begin for some patients, though I can't visually see any. 
# Actually i'm expecting this to be the case and should revisit the time. 

gg_conc <- ggplot(Study1Conc,aes(x=TIMEPOS,y=DV))+
  geom_line()+
  facet_grid(.~USUBJID)

# On another look at the column definitions, NT might be safer? 


gg_conc <- ggplot(Study1Conc,aes(x=NT,y=DV))+
  geom_line()+
  facet_grid(.~USUBJID)

# Results are identical, but note to self keep an eye on the time units when looking at study 2 / 3. 

# Log scale:

gg_conc <- ggplot(Study1Conc,aes(x=NT,y=DV))+
  geom_line()+
  facet_grid(.~USUBJID)+
  scale_y_continuous(trans="log10")

# Ok, add the dosing. I want to keep Dose/Conc in seperate dfs so call each individually. 


gg_conc <- ggplot()+
  geom_line(data=Study1Conc,aes(y=DV,x=NT))+
  geom_point(data=Study1Dose,aes(y=1,x=NT))+
  facet_grid(.~USUBJID)+
  scale_y_continuous(trans="log10")

# Keeping y on 1 for the dosing isn't neat, but i'm just visualizing the data at the moment. 
# Ideally I'd track this to conc somehow but the NTs are (obviously) different.Can't really think of an elegant 
# Solution right now. 


gg_conc <- ggplot()+
  geom_line(data=Study1Conc,aes(y=DV,x=NT))+
  geom_point(data=Study1Dose,aes(y=1,x=NT,color=TRTNAME))+
  facet_grid(.~USUBJID)+
  scale_y_continuous(trans="log10")+
  theme_bw()

# Would be good to re-label several aspects of the data - TRTNAME and USUBJID particularly. 
# So todo:
# Re-label and censor BLLOQ 

# What I really want to do is parse the data frame to create a neater d.f with just the study and the dose. 
# This can then be used to label the data properly. 

# Actually, there's a much better label for dose already existing:
gg_conc <- ggplot()+
  geom_line(data=Study1Conc,aes(y=DV,x=NT))+
  geom_point(data=Study1Dose,aes(y=1,x=NT,color=as.factor(DOSECpdA)))+
  facet_grid(.~USUBJID)+
  scale_y_continuous(trans="log10")+
  theme_bw()

# Much better! And easy to convert when we look at the combined therapy as well. 
# Need a system to rename the facet grid. Labeller is the best way of doing it (I think). 
# Use unique() to pull each individual value, then just label them 1, 2, 3 etc? 
# No, wait, i'm overcomplicating this - just use ID column instead of USUBJID! 

gg_conc <- ggplot()+
  geom_line(data=Study1Conc,aes(y=DV,x=NT))+
  geom_point(data=Study1Dose,aes(y=1,x=NT,color=as.factor(DOSECpdA)))+
  facet_grid(.~ID)+
  scale_y_continuous(trans="log10")+
  theme_bw()

# Clearly appearance-based edits to be made. Worth noting as well that 1:8 are "Vehicle" i.e. no drug. 

# Subsets the conc data Study1, Study2 etc. Functionally the same as the definition of study1Dose above,
# But this is better as it will pick up both Concs for Study3. Again dependent on grep which would pose
# difficulty with poorer quality / mislabeled data. 

# The other option is to use TRT? 

get_conc <- function(studyDat){
  
  StudyDose <- studyDat[grep("Concentration",studyDat$NAME),]
  
  return(StudyDose)
}

TRTA <- c(0:7)
TRTB <- c(0,8:14)
TRTC <- c(0,2:4,8:11,15:22)

# So then we'd do something like:
get_conc_TRT <- function(studyDat,trt){
  
  StudyDose <- studyDat %>% 
    filter(studyDat$TRT==c(trt))
 
  return(StudyDose) 
  
}

# Ah, but parasitaemia still has attached TRT values. So let's just get the conc using get_conc. 

gg_conc <- ggplot()+
  geom_line(data=get_conc(Study2),aes(y=DV,x=NT))+
  facet_grid(.~ID)+
  scale_y_continuous(trans="log10")+
  theme_bw()

# Get conc can be pulled straight into GGplot. 

get_dose <- function(studyDat){
  
  StudyDose <- studyDat[grep("Dose",studyDat$NAME),]
  
  # todo: mutate here to call "A" or "B" based on which one is >0 ?, then a new "AMT"
  # as AMT is defunct for any row that isn't a dose. 
  # Would make plotting the spaghetti plots much easier. 
  
  return(StudyDose)
}

# Note switching to AMT rather than calling CpdA/CpdB directly. 
# Need to consider how to handle this for Study3 but this is a good framework IMO. 

gg_conc <- ggplot()+
  geom_line(data=get_conc(Study2),aes(y=DV,x=NT))+
  geom_point(data=get_dose(Study2),aes(y=1,x=NT,color=as.factor(AMT)))+
  facet_grid(.~ID)+
  scale_y_continuous(trans="log10")+
  theme_bw()

gg_conc <- ggplot()+
  geom_line(data=get_conc(Study2),aes(y=DV,x=NT,color=as.factor(ID)))+
  scale_y_continuous(trans="log10")+
  theme_bw()

# Better to color by dose in all honesty. Can do this with doseCPdA / doseCPDB, just need to make sure R knows 
# Which one to call. 


# So functionalized versions work fine for Study 1 and 2. 

# Self: have a closer look at a single patient from Study 3. 

Study3[Study3$ID==140,]

# Looking for a "cheat" to pull get_conc and have R "know" which drug compound it is. 
# Or do I even have to do that? Can we just group geom_line by NAME ?

gg_conc <- ggplot()+
  geom_line(data=get_conc(Study3),aes(y=DV,x=NT, color=get_conc(Study3)$NAME))+
  facet_grid(.~ID)+
  scale_y_continuous(trans="log10")+
  theme_bw()

# Well, this certainly works, but there's a lot more patients so the facet grid layout is somewhat illegible. 

# One thing that is a little unclear to me is why there's monotherapy data included in Study 3 when that 
# data is being provided by Study 1 and 2. 

# Fun plot! 

gg_conc <- ggplot()+
  geom_line(data=get_conc(Study3),aes(y=DV,x=NT,color=as.factor(get_conc(Study3)$ID)))+
  scale_y_continuous(trans="log10")+
  facet_grid(NAME~.)+
  theme_bw()

# Ok, same function but with extra "dose cleaning". 
get_conc2 <- function(studyDat){
  
  StudyDose <- studyDat[grep("Concentration",studyDat$NAME),]
  
  # Need to a) check which concentration we're using and b) get it. 
  # Can't just pull a/b from pkpd data as for study 3 the combo has both. 
  # However, NAME does state CpdA/B so we can just grep, then pull from the according column.
  # Vector of all CpdA:
  # Note to self, stringr might be a better option? 
  
  CpdA<- StudyDose %>%
    filter(str_detect(NAME, "CpdA")) %>%
    mutate(Compound="CpdA",Dose=DOSECpdA)
    
  CpdB <- StudyDose %>%
    filter(str_detect(NAME, "CpdB")) %>%
    mutate(Compound="CpdB",Dose=DOSECpdB)
  
  # The tidy option seems better than grep. 
  
  # Bind them together (only relevant for study 3, but A and B should just add the empty row.)
  # Also, can also edit this function to give separate output if needed. 
  
  editedStudyDose <- rbind(CpdA,CpdB)
  
  # Later ill probably want to trim down to just the data. Essentially this means that for each
  # row (i.e., a drug conc reading), there will just be one dose (Dose) and a compound i.d (Compound)
  
  
  return(editedStudyDose)
}

gg_conc <- ggplot()+
  geom_line(data=get_conc2(Study3),aes(y=DV,x=NT,color=as.factor(get_conc2(Study3)$ID)))+
  scale_y_continuous(trans="log10")+
  facet_grid(Compound~.)+
  theme_bw()

# Everything works normally with the new function.
# 
# TODO:
# Separate Study 3 into Just A, just B and Combination. 
# LLOQ 
