#'treatment.effect.df
#'
#'
#'
#'@param df is the dataframe that contains the kuni data
#'@param treat is the treatment categories to split the dataframe into, dataframes for each treatment
#'
#'@author Elizabeth Hiroyasu
#'







#republican data frame
m = c(mean(rep_econgain), mean(rep_econloss), mean(rep_ecogain), mean(rep_ecoloss))
ATE=c(mean(rep_econgain)-mean(rep_control), mean(rep_econloss)-mean(rep_control), 
      mean(rep_ecogain)-mean(rep_control), mean(rep_ecoloss)-mean(rep_control))
treat_cat_rep = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
se = c(sd(rep_econgain)/sqrt(length(rep_econgain)), 
       sd(rep_econloss)/sqrt(length(rep_econloss)), sd(rep_ecogain)/sqrt(length(rep_ecogain)),
       sd(rep_ecoloss)/sqrt(length(rep_ecoloss)))
#ci<-ATE + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/N)*ATE*(1-ATE))
party_rep<-as.factor(rep(1, each=length(m)))

rep.df<-data.frame(treat_cat_rep, m, se, ATE, party_rep)
rep.df<-setNames(rep.df, c("treat_cat", "mean", "se", "ATE", "partyid"))