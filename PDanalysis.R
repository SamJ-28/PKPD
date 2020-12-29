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

# pull in plot function 

spaghetti_par <- function(Study){
  
  print("Takes get_par")
  gg_conc <- ggplot()+
    geom_line(data=Study,aes(y=DV,x=NT,color=as.factor(Study$ID)))+
    facet_grid(ID~.)
    theme_bw()
  
  return(gg_conc)
}
