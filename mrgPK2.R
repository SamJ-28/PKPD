# mrgsolve PK fit try 2

# https://github.com/metrumresearchgroup/ub-cdse-2019/blob/master/content/tools_optimization_indomethacin.md

# n.b. one compartment model assumes infusion? base mrgsolve.. 

library(mrgsolve)
library (dplyr)
library (nloptr)
library (ggplot2)

# Source functions
source("functions.R")
# Hardread data
PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")

# Read in drug concs:
Study1 <- get_individual_study(PKPDdata,"Study1")
# Drug conc 
Study1_CpdA_Conc <- export_conc_mono(Study1,"CpdA")
# Read in drug dose
Study1_dose <- get_dose_mono(Study1,"CpdA")

# Format data into a way that can be used by mrgsolve 
# i.e .   time conc evid cmt ID amt
Study1ID <- c(unique(Study1_CpdA_Conc$ID))

colnames(Study1_dose) <- c("ID","time","amt")
colnames(Study1_CpdA_Conc) <-c("ID","time","conc")
mrg_data <- data.frame(time=as.numeric(),conc=as.numeric(),evid=as.numeric(),cmt=as.numeric(),
                       ID=as.numeric(),amt=as.numeric())

# Loop to get data into a good format for mrgsolve. 
for(i in Study1ID){
  # add dose to mrg_data
  dose_subset <- Study1_dose %>%
    filter(ID==i) %>%
    mutate(evid=1,cmt=1,conc=NA) %>%
    relocate(time,conc,evid,cmt,ID,amt)
  
  
  # Now order and mutate conc, then rbind each ID, order by time.. 
  conc_subset <- Study1_CpdA_Conc %>%
    filter(ID==i) %>%
    mutate(evid=0,cmt=0,amt=0) %>%
    relocate(time,conc,evid,cmt,ID,amt)
  
  ID_subset <- rbind(dose_subset,conc_subset) %>%
    arrange(time,-evid)
  
  mrg_data <- rbind(mrg_data,ID_subset)
  
  
}

mrg_data9 <- filter(mrg_data,ID==9)

mrg_data22 <- filter(mrg_data,ID==22)
# Import obj function from kyle baron github:

obj <- function(p, theta, data, dv ="conc", pred = FALSE) {
  
  names(p) <- names(theta)
  
  p <- lapply(p,exp)
  
  mod <- param(mod, p)
  
  out <- mod %>% param(p) %>% mrgsim_d(data, output="df")
  
  if(pred) return(out)
  
  sqr <- (out[["CP"]] - data[[dv]])^2
  
  sum(sqr, na.rm=TRUE)
}

# least weighted sqaures
objlws <- function(p, theta, data, wt, pred = FALSE) {
  names(p) <- names(theta)
  p <- lapply(p,exp)
  out <- mod %>% param(p) %>% mrgsim_q(data, output="df")
  if(pred) return(out)
  return(sum(((out$CP - data[["conc"]])*wt)^2, na.rm=TRUE))
}

mod <- modlib("pk1")

param(mod)

theta <- log(c(CL = 10, V = 8, KA1 = 1))

#obj(theta,theta,mrg_data)

fit <- optim(par = theta, fn=obj, theta = theta, data=mrg_data9)

pred <- obj(fit$par, theta, mrg_data22, pred = TRUE)

mrg_data22$pred <- pred$CP


cmt_1_22 <-ggplot(data = mrg_data22) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(aes(time,pred),col="firebrick", lwd=1)

## 2 comp

mod <- modlib("pk2cmt")
theta <- log(c(CL = 2, V2 = 50, Q = 10, V3 = 50, KA1 = 1, KA2= 1))
fit <- optim(par = theta, fn=obj, theta = theta, data=mrg_data)

pred <- obj(fit$par, theta, mrg_data, pred = TRUE)
mrg_data$pred <- pred$CP

cmt_2 <- ggplot(data = mrg_data) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(aes(time,pred),col="firebrick", lwd=1)



mod <- modlib("pk3cmt")

theta <- log(c(CL = 2, V2 = 50, Q = 10, V3 = 50))
fit <- optim(par = theta, fn=obj, theta = theta, data=mrg_data)

pred <- obj(fit$par, theta, mrg_data, pred = TRUE)
mrg_data$pred <- pred$CP

cmt_3 <- ggplot(data = mrg_data) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(aes(time,pred),col="firebrick", lwd=1)


# Test new obj: objwls
dv <- mrg_data9[["conc"]]

fit_wt <- minqa::newuoa(par = theta, fn=objlws, theta = theta, data=mrg_data9, wt=1/dv)
objlws(fit_wt$par,theta,mrg_data9,dv)

pred <-  objlws(fit$par, theta, mrg_data9, wt = 1/dv, pred = TRUE)
predi <- objlws(theta,  theta, mrg_data9, wt = 1/dv, pred = TRUE)
predw <- objlws(fit_wt$par, theta, mrg_data9, wt = 1/dv, pred = TRUE) 


mrg_data9$pred <- pred$CP
mrg_data9$predi <- predi$CP
mrg_data9$predw <- predw$CP
head(mrg_data9)

pred <- distinct(data, time, .keep_all = TRUE)

ggplot(data = mrg_data9) + 
  geom_point(aes(time,conc)) + 
  scale_y_log10() + 
  geom_line(data=mrg_data9,aes(time,pred),col="black", lwd=1, alpha = 0.6) +
  geom_line(data=mrg_data9,aes(time,predi),col="darkgreen", lwd=1) + 
  geom_line(data = mrg_data9, aes(time,predw), col="firebrick", lwd = 1)

# todo: functionalize, 
# individual runs