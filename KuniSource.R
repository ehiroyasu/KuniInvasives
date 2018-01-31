###Source file for the KuniInvasives project
#This file loads the required packages and reads in the data

library(ggplot2)
library(Rmisc)
library(psych)

kuni_df <- read.csv("./data/Processed/kunisurvey_coded_march16.csv", h=T)
