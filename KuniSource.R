###Source file for the KuniInvasives project
#This file loads the required packages and reads in the data
# library(devtools)
# library(roxygen2)
# install("GainLossPackage")
# library(GainLossPackage)
library(ggplot2)
library(Rmisc)

kuni_df <- read.csv("./data/Processed/kunisurvey_coded_march16.csv", h=T)
# 
# df.treat<-function(df){
#   attach(df)
#   #separating by each treatment type
#   df$treat[ecogain==1] <- "ecogain"
#   df$treat[ecoloss==1] <- "ecoloss"
#   df$treat[econgain==1] <- "econgain"
#   df$treat[econloss==1] <- "econloss"
#   df$treat[control==1] <- "control"
#   
#   #Separating by the ecology vs econ pooled treatments
#   df$ecoecon[ecogain==1 | ecoloss==1] <- "eco"
#   df$ecoecon[econloss==1 | econgain==1] <- "econ"
#   df$ecoecon[control==1] <- "control"
#   
#   #Separating by the pooled gain vs loss treatments
#   df$gainloss[ecogain==1 | econgain==1] <- "gain"
#   df$gainloss[ecoloss==1 | econloss==1] <- "loss"
#   df$gainloss[control==1] <- "control"
#   
#   return(df)
# }

# treat_func<-function(df){
#   
#   summary(ecogain <- support_binary[treat_cat=="ecogain"], na.rm=T)
#   summary(ecoloss <- support_binary[treat_cat=="ecoloss"], na.rm=T)
#   summary(econgain <- support_binary[treat_cat=="econgain"], na.rm=T)
#   summary(econloss <- support_binary[treat_cat=="econloss"], na.rm=T)
#   summary(control <- support_binary[treat_cat=="control"], na.rm=T)
#   
#   m = c(mean(econgain), mean(econloss), mean(ecogain), mean(ecoloss))
#   ATE=c(mean(econgain)-mean(control), mean(econloss)-mean(control), 
#         mean(ecogain)-mean(control), mean(ecoloss)-mean(control))
#   treat_cat = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
#   se = c(sd(econgain)/sqrt(length(econgain)), 
#          sd(econloss)/sqrt(length(econloss)),
#          sd(ecogain)/sqrt(length(ecogain)),
#          sd(ecoloss)/sqrt(length(ecoloss)))
#   
#   treat.df<-data.frame(treat_cat, m, se, ATE)
#   treat.df<-setNames(treat.df, c("treat_cat", "mean", "se", "ATE"))
#   
#   return(treat.df)
#   
# }