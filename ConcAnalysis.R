# Drug conc comparison script

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
