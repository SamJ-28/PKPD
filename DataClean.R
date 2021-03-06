# R script to clean and visually analyze data
# Sam Jones

# To the reader: This file is the one I used as a "scrapbook" of sorts when building code 
# to clean and visualize the provided data. I'll keep my "thought process" and general workflow
# included- as this will be used to assess me I believe it will be beneficial you have some idea 
# of what goes through my head as I approach a task! ( this may be "rambling" at points...)
# If you've ended up here but want the "clean" code for the final version in the presentation,
# please check elsewhere in my github.

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

spaghetti_conc <- function(Study){
  
  gg_conc <- ggplot()+
    geom_line(data=get_conc2(Study),aes(y=DV,x=NT,color=as.factor(get_conc2(Study)$ID)))+
    scale_y_continuous(trans="log10")+
    facet_grid(.~Compound)+
    theme_bw()
  
  return(gg_conc)
}

# Separated conc allows for easy labelling of doses. Need to think about how to do that 
# With the spaghetti plots (i.e., separate lines by individual but color by doses.)

seperated_conc <- function(Study){
  
  gg_conc <- ggplot()+
    geom_line(data=get_conc2(Study),aes(y=DV,x=NT,color=as.factor(get_conc2(Study)$Dose)))+
    scale_y_continuous(trans="log10")+
    facet_grid(ID~Compound)+
    theme_bw()

  return(gg_conc)
}

# Grouping does the trick.

spaghetti_dose_conc <- function(Study){
  
  gg_conc <- ggplot()+
    geom_line(data=get_conc2(Study),aes(y=DV,x=NT,group=as.factor(get_conc2(Study)$ID),color=as.factor(get_conc2(Study)$Dose)))+
    scale_y_continuous(trans="log10")+
    facet_grid(.~Compound)+
    theme_bw()
  
  return(gg_conc)
}


# Let's do LLOQ first as this should be a fairly easy task, I am simply filtering out rows 
# based on the contents of the IGNORE column. I'm a little reluctant to do this in case I 
# explore other ways of dealing with LLOQ data later... but I also might forget and it's
# easy enough to restore the data with a different function (get_conc_2 or etc.)

# Old annotations removed, see get_conc2! 
# Note that get_conc2() has different entries in IGNORE.
# "BLLOQ:::(M1)" in Study1
#  "Concentration:::is:::Below:::LLOQ" in Study 3. 

get_conc3 <- function(studyDat){
  
  StudyDose <- studyDat[grep("Concentration",studyDat$NAME),]

  CpdA<- StudyDose %>%
    filter(str_detect(NAME, "CpdA")) %>%
    mutate(Compound="CpdA",Dose=DOSECpdA)
  
  CpdB <- StudyDose %>%
    filter(str_detect(NAME, "CpdB")) %>%
    mutate(Compound="CpdB",Dose=DOSECpdB)

  editedStudyDose <- rbind(CpdA,CpdB) %>%
    filter(!str_detect(IGNORE,"LLOQ"))
  
  
  return(editedStudyDose)
}


gg_conc1 <- spaghetti_dose_conc(get_conc2(Study1))
gg_conc2 <- spaghetti_dose_conc(get_conc3(Study1))

# That handles it nicely (imho). Dependant on LLOQ being a char string in the IGNORE col, but that
# does seem to be a theme with this data-set. 

# TODO: Make this pull the monotherapies. 

get_conc_combi <- function(dat,which_run){
  
  print("Takes the input of get_conc3(study)")
  if(which_run=="CpdA"){
    dat_test <- dat %>%
      filter(str_detect(TRTNAME, "CpdA") & !str_detect(TRTNAME,"CpdB"))
  }
  
  if(which_run=="CpdB"){
    dat_test <- dat %>%
      filter(str_detect(TRTNAME, "CpdB") & !str_detect(TRTNAME,"CpdA"))
  }

  if(which_run=="Combination"){
    dat_test <- dat %>%
      filter(str_detect(TRTNAME, "CpdB") & str_detect(TRTNAME,"CpdA"))
  }
  
  return(dat_test)  
}


gg_conc3 <- spaghetti_dose_conc(get_conc_combi(get_conc3(Study3),"Combination"))

# Now what I really want to do is see how the monotherapy portion of Study 3 compares to 1/2. 
gg_conc4 <- spaghetti_dose_conc(get_conc_combi(get_conc3(Study3),"CpdA"))
gg_conc5 <- spaghetti_dose_conc(get_conc_combi(get_conc3(Study1),"CpdA"))
gg_conc6 <- spaghetti_dose_conc(get_conc3(Study1))

# All of these work. Now for a quick test... 
test <- get_conc3(Study1) %>%
  filter(Dose==5 | Dose== 10 | Dose ==25)

gg_conc7 <- spaghetti_dose_conc(test)

# So pretty similar outcomes, and we have functions for extracting conc and creating ggplots. 
# I want the parasitaemia data now in a similar fashion. 
# Then I want to extract DATA in a form that's reasonable to be read by mrgsolve:

# useful ref: https://mhismail.github.io/2018-01-28-pk-fit/ 

# Parasitaemia:
# So this can use the same code really as the conc. 
# But let's switch the grep to str_detect. Should also need less in the function
# as no different doses to distinguish between, but still want to pull the Cpds for study 3. 

# As a note, get Conc 3 and get_conc_combi could probably be combined. 

# As far as I can tell, there's no IGNORE criteria, but good practice to include a catch anyway. 
get_par <- function(studyDat, which_treatment){
  print("Takes Study1, Study2, etc..")
  
  Study_par <- studyDat %>%
    filter(str_detect(NAME, "Parasitemia")) 
  
  # Can we pipe these multiple if statements?
  if(which_treatment=="CpdA"){
    Study_par<- Study_par %>%
      filter(str_detect(TRTNAME, "CpdA") & !str_detect(TRTNAME,"CpdB"))
  }
  
  if(which_treatment=="CpdB"){
    Study_par<- Study_par %>%
        filter(str_detect(TRTNAME, "CpdB") & !str_detect(TRTNAME,"CpdA"))
    
  }
  
  if(which_treatment=="Combination"){
    Study_par<- Study_par %>%
      filter(str_detect(TRTNAME, "CpdB") & str_detect(TRTNAME,"CpdA"))
  }
  
  print(class(Study_par))
  
  return(data.frame(Study_par))
}


spaghetti_par <- function(Study){
  
  print("Takes get_par")
  gg_conc <- ggplot()+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID)))+
    theme_bw()
  
  return(gg_conc)
}

# Probably want to plot parasitaemia by dose. Difficult to do, though. Facet? 

spaghetti_par_facet <- function(Study){
  
  print("Takes get_par")
  gg_conc <- ggplot()+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID)))+
    theme_bw()+
    facet_grid(DOSECpdA~DOSECpdB)
  
  return(gg_conc)
}

# Honestly quite an interesting plot. 
gg_par<-spaghetti_par_facet(get_par(Study3,"Combination"))+
  ylab("Ln (% of IRBCs)")+
  xlab("Time (hours)")+
  geom_vline(xintercept=672, linetype="dashed", color="red")

 
gg_par<-spaghetti_par_facet(get_par(Study3,"CpdA"))+
  ylab("Ln (% of IRBCs)")+
  xlab("Time (hours)")+
  geom_vline(xintercept=672, linetype="dashed", color="red")


# Didn't think about the facet grid being set up that way. Easy enough to change.. 

gg_par<-spaghetti_par_facet(get_par(Study3,"CpdB"))+
  ylab("Ln (% of IRBCs)")+
  xlab("Time (hours)")+
  geom_vline(xintercept=672, linetype="dashed", color="red")+
  facet_grid(DOSECpdB~DOSECpdA)

# TODO: Parasitaemia is a %, make sure you understand what's happening. 
# TODO: Include ignore catch anyway. 
# TODO: Think about final plots. 

# Mice are engrated with 40% human rbcs - infected when they have 40%. 
# Dose starts when infected have 1% parasitaemia (i.e., irbc)
# parasitaemia is expressed as % of irbcs among the human irbcs. 
# values of parasitaemia in DV are "ln transformed. "
# Document states "column NAME=Log10(Parasitemia))."
# Can't see the log 10 - assume it's ln and check in the new year when MMV reopens. 
# Patients "cured" when parasites below detectable level. Not clear to me from the document 
# What this level would be in mice.. probably not safe to assume it's censored as the length
# of follow-up is very variable. 

# Start thinking about data extraction for the PK model. 

# Ok, so for the PK model we're after drug conc and dose / time. 
# Both are pretty simple in terms of input to mrgsolve, just ID, dose, time. 
# mrgsolve function expand.ev() handles the other stuff that's needed..

# So need new functions, "export_conc" and "export_dose" that take the outputs of 
# get_conc and get_dose, trim the un-needed parameters off, and save them. 

# 

# function to export drug concs for use  in mrgsolve
# todo - maybe: one function for both the combination and monotherapies would be nice
# but the combination therapy compounds need to be split to make my life easy in mrgsolve.
# so ease might have to triumph "neat programming" here. 

export_conc_mono <- function(Study,which_run){
  
  # which_run is only needed for study 3
  if(missing(which_run)){
    
    # load drug data
    conc_data <- get_conc3(Study) %>%
      select(ID,NT,DV)
    
  }
  # Study 3 needs to specify which compound
  # As it has 2x monotherapies and a combination therapy (that itself has 2x drug concs)
  else{
    
    print("don't use this function for getting the combination data, it needs to be split")    
    conc_data <- get_conc_combi(get_conc3(Study),which_run)%>%
      
      select(ID,NT,DV)
    
    
  }
  
  return(conc_data)
  
}


export_conc_combi <- function(Study,which_compound){
  
  
  conc_data <- get_conc_combi(get_conc3(Study),"Combination")%>%
    filter(Compound==which_compound)%>%
    select(ID,NT,DV)
  
  
  
  return(conc_data)
  
}
# As of 22/12 mostly stopping on this exploratory script and moving functions to seperate files for plotting and PK models
