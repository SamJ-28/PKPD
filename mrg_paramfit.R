# run mrg model independently for bugtesting.. 

library(tidyverse)
library(ggplot2)
library(stringr)
library(mrgsolve)


PKPDdata<-read.csv("D:/SAM/Documents/Interview/All_PKPDdata.csv")


source("functions.R")



mod <- modlib("pk1")
theta <- log(c(CL = 100, V = 80, KA = 10))



all_ID <- unique(data$ID)
loop_ID <- 0


for( i in all_ID){
  
  loop_ID <- loop_ID+1
  subset_data <- filter(data,ID==i)
  
  if(optimizer=="OLS"){
    fit <- optim(par = theta, fn=objOLS, theta = theta, data=subset_data)
    pred <- objOLS(fit$par, theta, subset_data, pred = TRUE)
  }
  
  if(optimizer=="LWS"){
    dv <- subset_data[["conc"]]
    # add weighting factor.. 
    fit <- minqa::newuoa(par = theta, fn=objLWS, theta = theta, data=subset_data, wt=1/dv)
    pred <- objLWS(fit$par, theta, subset_data, wt = 1/dv, pred = TRUE)
  }
  
}